// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node.merge

import osiris._
import container.companion.ContainerCompanion
import pin.{MatrixPin, Pin, Socket}
import osiris.evaluator.Environment
import osiris.morphism.product
import osiris.pin.node.Node
import osiris.pin.node.split.RowSplit
import osiris.shape.Shape
import vector.Vector
import vector.space.MatrixSpace

import scala.collection.mutable

class RowMerge[I,J,S](space:MatrixSpace[I,J,S]) extends Node {

  private val socketSpace:ContainerCompanion[I,In] = space.outer.shape --> [In]()
  val in = socketSpace(new In(_))

  val pins = Set(out)
  val sockets = in.iterator.toSet //TODO

  def eval(environment: Environment): Unit = {
    environment.put(out, space.rows(i => environment(in(i).pin.get)))
  }

  def rowWise[II](shape: Shape[II],matrixifiedPins: mutable.Map[Pin[_,_],MatrixPin[II,_,_]]): Unit = {
    import morphism._
    val oo = ((shape*space.inner.shape)*space.outer.shape --> space.scalarSpace).cols((i:I) =>
      matrixifiedPins(in(i).pin.get).asInstanceOf[MatrixPin[II,J,S]]
    ).permute(
        product.assocLeft(shape,space.inner.shape,space.outer.shape) o
          product.rmap(shape,product.commute(space.outer.shape,space.inner.shape))
    ).asMatrix[II,(I,J),(II,(I,J))]
    matrixifiedPins(out) = oo
  }

  object out extends MatrixPin[I,J,S] {

    val space = RowMerge.this.space

    val node = RowMerge.this

  }

  class In(i:I) extends Socket[J,S] {

    val space = RowMerge.this.space.inner
    val node = RowMerge.this

    def evaluateFeedback(environment: Environment): Unit = {
      environment.putFeedback(pin.get,environment.feedback(out).asMatrix[I,J,(I,J)].row(i))
    }

    def feedbackDependencies = Set(Right(out))

  }

}