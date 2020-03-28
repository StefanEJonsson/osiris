// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node.replace

import osiris._
import osiris.evaluator.Environment
import osiris.pin.{MatrixPin, Pin, Socket}
import osiris.pin.node.Node
import osiris.shape.Shape
import osiris.vector.Single
import osiris.vector.space.VectorSpace

import scala.collection.mutable

/**
  * Has two sockets (one vector and one singleton) and one pin (vector). The output is the same as the input vector but
  * with one element replaced by the singleton.
  */
class Replace[I,S](space:VectorSpace[I,S],i:I) extends Node {

  val sockets = Set(in,replacement)
  val pins = Set(out)

  def eval(environment: Environment): Unit = {
    val value = environment(replacement.pin.get)
    val res = environment(in.pin.get).replace(i,value())
    environment.put(out,res)
  }

  def rowWise[II](shape:Shape[II],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[II,_,_]]): Unit = {
    import morphism._
    val rr = new ReplaceCol[II,I,S](shape*space.shape --> space.scalarSpace,i)
    matrixifiedPins(in.pin.get).asInstanceOf[MatrixPin[II,I,S]] ->- rr.in
    matrixifiedPins(replacement.pin.get).asInstanceOf[MatrixPin[II,Unit,S]].permute(product.putLeft(shape)) ->- rr.replacement
    matrixifiedPins(out) = rr.out
  }

  object in extends Socket[I,S] {

    val space = Replace.this.space
    val node = Replace.this

    def evaluateFeedback(environment: Environment): Unit = {
      val feedback = environment.feedback(out).replace(i,space.scalarSpace.zero)
      environment.putFeedback(pin.get,feedback)
    }

    def feedbackDependencies: Set[Either[Pin[_,_],Pin[_,_]]] = Set(Right(out))

  }

  object replacement extends Socket[Unit,S] {

    val space = I --> Replace.this.space.scalarSpace
    val node = Replace.this

    def evaluateFeedback(environment: Environment): Unit = {
      val feedback = new Single(environment.feedback(out)(i))
      environment.putFeedback(pin.get,feedback)
    }

    def feedbackDependencies: Set[Either[Pin[_,_],Pin[_,_]]] = Set(Right(out))

  }

  object out extends Pin[I,S] {

    val space = Replace.this.space

    val node = Replace.this

  }

}
