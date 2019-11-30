// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node.split

import osiris._
import container.companion.ContainerCompanion
import pin.{MatrixPin, Pin, Socket}
import osiris.evaluator.Environment
import osiris.pin.node.Node
import osiris.pin.node.merge.RowMerge
import osiris.shape.Shape
import vector.space.MatrixSpace

import scala.collection.mutable

class RowSplit[I,J,S](space:MatrixSpace[I,J,S]) extends Node {

  private val pinSpace:ContainerCompanion[I,Pin[J,S]] = space.outer.shape --> [Pin[J,S]]()

  private val out = pinSpace(new Out(_))

  val pins = out.iterator.toSet
  val sockets = Set(in)

  def eval(environment: Environment): Unit = {
    space.outer.shape.foreach( i => environment.put(out(i), environment(in.pin.get).asMatrix[I,J,(I,J)].row(i)))
  }

  def rowWise[II](shape:Shape[II],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[II,_,_]]): Unit = {
    import morphism._
    val outermost = shape --> space.scalarSpace
    val ss = new RowSplit(space.outer*(space.inner*outermost))
    val min:Pin[(I,(J,II)),S] =
      matrixifiedPins(in.pin.get).asInstanceOf[MatrixPin[II,(I,J),S]].permute (
        product.commute(space.shape,shape) o
        product.assocLeft(space.outer.shape,space.inner.shape,shape)
      )
    min ->- ss.in
    for (i <- space.outer.shape) {
      matrixifiedPins(out(i)) = ss(i).asMatrix.transpose
    }
  }

  object in extends Socket[(I,J),S] {

    val space:MatrixSpace[I,J,S] = RowSplit.this.space
    val node = RowSplit.this

    def evaluateFeedback(environment: Environment): Unit = {
      environment.putFeedback(pin.get,space.rows(i => environment.feedback(out(i))))
    }

    def feedbackDependencies = out.iterator.map(Right(_)).toSet

  }

  def apply(i:I):Pin[J,S] = out(i)

  class Out(i:I) extends Pin[J,S] {

    val space = RowSplit.this.space.inner

    val node = RowSplit.this

  }


}
