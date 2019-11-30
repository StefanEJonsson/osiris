// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node.cast

import osiris._
import pin.{EmptyPin, MatrixPin, Pin, Socket}
import osiris.evaluator.Environment
import osiris.pin.node.Node
import osiris.shape.Shape
import osiris.vector.space.{EmptySpace, VectorSpace}
import vector.{Empty, Vector}

import scala.collection.mutable

class EmptyCast[S](space:VectorSpace[Nothing,S]) extends Node {

  val sockets = Set(in)
  val pins = Set(out)

  def eval(environment: Environment): Unit = {
    environment.put(out,new Empty[S](space.scalarSpace))
  }

  def rowWise[I](shape:Shape[I],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[I,_,_]]): Unit = {
    matrixifiedPins(out) = matrixifiedPins(in.pin.get)
  }

  object in extends Socket[Nothing,S] {

    val space = EmptyCast.this.space
    val node = EmptyCast.this

    def evaluateFeedback(environment: Environment): Unit = {
      environment.putFeedback(pin.get,new Empty[S](space.scalarSpace))
    }

    def feedbackDependencies = Set()

  }

  object out extends EmptyPin[S] {

    val space = EmptyCast.this.space.asEmptySpace

    val node = EmptyCast.this

  }

}