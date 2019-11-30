// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node.cast

import osiris.+
import osiris.evaluator.Environment
import osiris.pin.node.Node
import osiris.pin.{MatrixPin, PairPin, Pin, Socket}
import osiris.shape.Shape
import osiris.vector.{Pair, Vector}
import osiris.vector.space.VectorSpace

import scala.collection.mutable

class PairCast[L,R,S](space:VectorSpace[Either[L,R],S]) extends Node {

  val sockets = Set(in)
  val pins = Set(out)

  def eval(environment: Environment): Unit = {
    environment.put(out,environment(in.pin.get).asPair[L,R,+[L,R]])
  }

  def rowWise[I](shape:Shape[I],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[I,_,_]]): Unit = {
    matrixifiedPins(out) = matrixifiedPins(in.pin.get)
  }

  object in extends Socket[Either[L,R],S] {

    val space = PairCast.this.space
    val node = PairCast.this

    def evaluateFeedback(environment: Environment): Unit = {
      environment.putFeedback(pin.get,environment.feedback(out))
    }

    def feedbackDependencies = Set(Right(out))

  }

  object out extends PairPin[L,R,S] {

    val space = PairCast.this.space.asPairSpace

    val node = PairCast.this

  }

}
