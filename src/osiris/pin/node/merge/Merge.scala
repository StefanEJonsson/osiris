// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node.merge

import osiris._
import container.companion.ContainerCompanion
import pin.{MatrixPin, Pin, Socket}
import osiris.evaluator.Environment
import osiris.pin.node.Node
import osiris.shape.Shape
import vector.Single
import vector.space.VectorSpace

import scala.collection.mutable

/**
  * Has N sockets that take singleton pins. Has one pin of vector type. The output is identical to the input but with
  * he singletons collected into a vector.
  */
class Merge[I,S](space:VectorSpace[I,S]) extends Node {

  private val socketSpace:ContainerCompanion[I,In] = space.shape --> [In]()

  val in = socketSpace(new In(_))

  val pins = Set(out)
  val sockets = in.iterator.toSet   //TODO change specification so that sockets doesn't have to be set (just any collection)

  def eval(environment: Environment): Unit = {
    environment.put(out,space.apply( (i:I) => environment(in(i).pin.get).asSingle.value ))
  }

  def rowWise[II](shape:Shape[II],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[II,_,_]]): Unit = {
    val oo = (shape * space.shape --> space.scalarSpace).cols((i:I) =>
      matrixifiedPins(in(i).pin.get).asInstanceOf[MatrixPin[II,Unit,S]].permute(morphism.product.putLeft(shape))
    )
    matrixifiedPins(out) = oo
  }

  object out extends Pin[I,S] {

    val space = Merge.this.space

    val node = Merge.this

  }

  class In(i:I) extends Socket[Unit,S] {

    val space = I --> Merge.this.space.scalarSpace
    val node = Merge.this

    def evaluateFeedback(environment: Environment): Unit = {
      environment.putFeedback(pin.get,new Single(environment.feedback(out)(i)))
    }

    def feedbackDependencies = Set(Right(out))

  }

}