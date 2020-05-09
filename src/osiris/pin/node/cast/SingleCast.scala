// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node.cast

import osiris._
import osiris.evaluator.environment.VectorEnvironment
import pin.{MatrixPin, Pin, SinglePin, Socket}
import osiris.pin.node.Node
import osiris.shape.Shape
import vector.space.VectorSpace

import scala.collection.mutable

class SingleCast[S](space:VectorSpace[Unit,S]) extends Node {

  val sockets = Set(in)
  val pins = Set(out)

  def eval(environment: VectorEnvironment): Unit = {
    environment.putValue(out,environment(in.pin.get).asSingle)
  }

  def rowWise[I](shape:Shape[I],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[I,_,_]]): Unit = {
    matrixifiedPins(out) = matrixifiedPins(in.pin.get)
  }

  object in extends Socket[Unit,S] {

    val space = SingleCast.this.space
    val node = SingleCast.this

    def evaluateFeedback(environment: VectorEnvironment): Unit = {
      environment.putFeedback(pin.get,environment.feedback(out))
    }

    def feedbackDependencies = Set(Right(out))

  }

  object out extends SinglePin[S] {

    val space = SingleCast.this.space.asSingleSpace

    val node = SingleCast.this

  }

}