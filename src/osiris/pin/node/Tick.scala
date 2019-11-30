// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node

import osiris.evaluator.Environment
import osiris.pin.{MatrixPin, Pin, Socket}
import osiris.shape.Shape
import osiris.vector.Vector
import osiris.vector.space.VectorSpace

import scala.collection.mutable

class Tick[I,S](space:VectorSpace[I,S],discountFactor:S) extends Node {

  val sockets = Set(in)
  val pins = Set(out)

  def eval(environment: osiris.evaluator.Environment): Unit = {
    environment.put(out,environment(in.pin.get))
  }

  def rowWise[II](shape:Shape[II],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[II,_,_]]): Unit = {
    val tt = new Tick(shape*space.shape --> space.scalarSpace,discountFactor)
    matrixifiedPins(in.pin.get).asInstanceOf[MatrixPin[II,I,S]] ->- tt.in
    matrixifiedPins(out) = tt.out.asMatrix
  }

  object in extends Socket[I,S] {

    val space = Tick.this.space
    val node = Tick.this

    def evaluateFeedback(environment: Environment): Unit = {
      environment.putFeedback(pin.get,environment.feedback(out)*discountFactor)
    }

    def feedbackDependencies = Set(Right(out))

  }

  object out extends Pin[I,S] {

    val space = Tick.this.space

    val node = Tick.this

  }



}
