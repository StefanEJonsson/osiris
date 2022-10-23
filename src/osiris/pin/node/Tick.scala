// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node

import osiris.evaluator.environment.VectorEnvironment
import osiris.pin.{MatrixPin, Pin, Socket}
import osiris.shape.Shape
import osiris.vector.Vector
import osiris.vector.space.VectorSpace

import scala.collection.mutable

/**
  * Works like the identity function during forward propagation but during backpropagation it multiplies the feedback
  * by a discount factor between zero and one.
  *
  * This is useful when preventing gradients from exploding to infinity in large Recurrent Neural Networks or when
  * studying the effects of short sightedness.
  *
  * Intuitively, every Tick is supposed to represent a point in time. Every node that occurs before the tick in the
  * computation graph represents decisions that are made before that point in time. Every objective that occurs after
  * the tick in the computation graph represents a consequence that occurs after that point in time. The discount factor
  * represents the fact that we have a tendency to care less about the consequences of a decision if there is a time
  * delay between the decision and the consequence. If one tick is introduced for every unit of time, then the feedback
  * gets multiplied by the discount factor once for every unit of time that separates the action from the consequence,
  * making the feedback go to zero as the delay goes to infinity.
  */
class Tick[I,S](val space:VectorSpace[I,S],val discountFactor:S) extends Node {

  val sockets = Set(in)
  val pins = Set(out)

  def eval(environment: VectorEnvironment): Unit = {
    environment.putValue(out,environment(in.pin.get))
  }

  def rowWise[II](shape:Shape[II],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[II,_,_]]): Unit = {
    val tt = new Tick(shape*space.shape --> space.scalarSpace,discountFactor)
    matrixifiedPins(in.pin.get).asInstanceOf[MatrixPin[II,I,S]] ->- tt.in
    matrixifiedPins(out) = tt.out.asMatrix
  }

  object in extends Socket[I,S] {

    val space = Tick.this.space
    val node = Tick.this

    def evaluateFeedback(environment: VectorEnvironment): Unit = {
      environment.putFeedback(pin.get,environment.feedback(out)*discountFactor)
    }

    def feedbackDependencies = Set(Right(out))

  }

  object out extends Pin[I,S] {

    val space = Tick.this.space

    val node = Tick.this

  }



}
