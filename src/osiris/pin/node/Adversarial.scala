// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node

import osiris.evaluator.Environment
import osiris.pin.{MatrixPin, Pin, Socket}
import osiris.shape.Shape
import osiris.vector.space.VectorSpace

import scala.collection.mutable

class Adversarial[I,S](space:VectorSpace[I,S]) extends Node {

  val sockets = Set(in)
  val pins = Set(out)

  def eval(environment: osiris.evaluator.Environment): Unit = {
    environment.put(out,environment(in.pin.get))
  }

  def rowWise[II](shape:Shape[II],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[II,_,_]]):Unit = {
    val AA = new Adversarial(shape*space.shape --> space.scalarSpace)
    matrixifiedPins(in.pin.get).asInstanceOf[MatrixPin[II,I,S]] ->- AA.in
    matrixifiedPins(out) = AA.out.asMatrix
  }

  object in extends Socket[I,S] {

    val space:VectorSpace[I,S] = Adversarial.this.space
    val node = Adversarial.this

    def evaluateFeedback(environment: Environment): Unit = {
      environment.putFeedback(pin.get,-environment.feedback(out))
    }

    def feedbackDependencies:Set[Either[Pin[_,_], Pin[_,_]]] = Set(Right(out))

  }

  object out extends Pin[I,S] {

    val space = Adversarial.this.space

    val node = Adversarial.this

  }

}
