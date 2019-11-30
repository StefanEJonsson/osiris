// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node.cast

import osiris.evaluator.Environment
import osiris.pin.node.Node
import osiris.pin.{MatrixPin, Pin, Socket}
import osiris.shape.Shape
import osiris.vector.space.VectorSpace

import scala.collection.mutable

class MatrixCast[I,J,S](space:VectorSpace[(I,J),S]) extends Node {

  val sockets = Set(in)
  val pins = Set(out)

  def eval(environment: Environment): Unit = {
    environment.put(out,environment(in.pin.get).asMatrix[I,J,(I,J)])
  }

  def rowWise[I](shape:Shape[I],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[I,_,_]]): Unit = {
    matrixifiedPins(out) = matrixifiedPins(in.pin.get)
  }

  object in extends Socket[(I,J),S] {

    val space = MatrixCast.this.space

    val node = MatrixCast.this

    def evaluateFeedback(environment: Environment): Unit = {
      environment.putFeedback(pin.get,environment.feedback(out))
    }

    def feedbackDependencies = Set(Right(out))

  }

  object out extends MatrixPin[I,J,S] {

    val space = MatrixCast.this.space.asMatrixSpace

    val node = MatrixCast.this

  }

}
