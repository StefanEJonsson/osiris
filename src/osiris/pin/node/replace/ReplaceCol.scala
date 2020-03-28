// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node.replace

import osiris.evaluator.Environment
import osiris.pin.{MatrixPin, Pin, Socket}
import osiris.pin.node.Node
import osiris.shape.Shape
import osiris.vector.space.MatrixSpace

import scala.collection.mutable

/**
  * Has two sockets (one of matrix type and one of vector type) and one pin (matrix type). The ouput is the same as the
  * input matrix but with one column replaced with the input vector.
  */
class ReplaceCol[I,J,S](space:MatrixSpace[I,J,S],j:J) extends Node {

  val sockets = Set(in,replacement)
  val pins = Set(out)

  def eval(environment: Environment): Unit = {
    val value = environment(replacement.pin.get)
    val res = environment(in.pin.get).asMatrix.replaceCol(j,value)
    environment.put(out,res)
  }

  def rowWise[II](shape:Shape[II],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[II,_,_]]): Unit = {
    import osiris.morphism._
    val rr = new ReplaceCol(((shape --> space.scalarSpace) * space.outer)*space.inner,j)
    val min =
      matrixifiedPins(in.pin.get).asInstanceOf[MatrixPin[II,(I,J),S]].permute(
        product.assocRight(shape,space.outer.shape,space.inner.shape)
      )
    val mrep = matrixifiedPins(replacement.pin.get).asInstanceOf[MatrixPin[II,I,S]]
    min ->- rr.in
    mrep ->- rr.replacement
    matrixifiedPins(out) =
      rr.out.permute(
        product.assocLeft(shape,space.outer.shape,space.inner.shape)
      ).asMatrix[II,(I,J),(II,(I,J))]
  }

  object in extends Socket[(I,J),S] {

    val space = ReplaceCol.this.space
    val node = ReplaceCol.this

    def evaluateFeedback(environment: Environment): Unit = {
      val feedback = environment.feedback(out).asMatrix[I,J,(I,J)].replaceCol(j,space.outer.zeros)
      environment.putFeedback(pin.get,feedback)
    }

    def feedbackDependencies: Set[Either[Pin[_,_],Pin[_,_]]] = Set(Right(out))

  }

  object replacement extends Socket[I,S] {

    val space = ReplaceCol.this.space.outer
    val node = ReplaceCol.this

    def evaluateFeedback(environment: Environment): Unit = {
      val feedback = environment.feedback(out).asMatrix[I,J,(I,J)].col(j)
      environment.putFeedback(pin.get,feedback)
    }

    def feedbackDependencies: Set[Either[Pin[_,_],Pin[_,_]]] = Set(Right(out))

  }

  object out extends MatrixPin[I,J,S] {

    val space = ReplaceCol.this.space

    val node = ReplaceCol.this

  }

}
