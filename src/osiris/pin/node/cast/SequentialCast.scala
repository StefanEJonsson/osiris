// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node.cast

import osiris.evaluator.Environment
import osiris.pin.node.Node
import osiris.pin.{MatrixPin, Pin, SequentialPin, Socket}
import osiris.shape.Shape
import osiris.vector.{Sequential, Vector}
import osiris.vector.space.VectorSpace

import scala.collection.mutable

class SequentialCast[S](space:VectorSpace[Int,S]) extends Node {

  val sockets = Set(in)
  val pins = Set(out)

  def eval(environment: Environment): Unit = {
    environment.put(out,environment(in.pin.get).asSequential)
  }

  def rowWise[I](shape:Shape[I],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[I,_,_]]): Unit = {
    matrixifiedPins(out) = matrixifiedPins(in.pin.get)
  }

  object in extends Socket[Int,S] {

    val space = SequentialCast.this.space
    val node = SequentialCast.this

    def evaluateFeedback(environment: Environment): Unit = {
      environment.putFeedback(pin.get,environment.feedback(out))
    }

    def feedbackDependencies = Set(Right(out))
  }

  object out extends SequentialPin[S] {

    val space = SequentialCast.this.space.asSequentialSpace

    val node = SequentialCast.this

  }

}