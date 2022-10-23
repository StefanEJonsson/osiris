// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris

import osiris.evaluator.environment.VectorEnvironment
import osiris.pin.{MatrixPin, Pin, Socket}
import osiris.pin.node.Node
import osiris.shape.Shape
import osiris.vector._
import osiris.vector.space.SingleSpace

import scala.collection.mutable

/**
  * Defines an objective (goal) for a machine learning model. When a pin is connected to this socket, that pin becomes
  * an objective and is optimized during training.
  *
  * Positive strengths correspond to maximization and negative strengths correspond to minimization. When multiple
  * objectives are used in the same model, a linear combination of the objectives is optimized. The coefficients in the
  * linear combination are given by the strengths.
  */
class Objective[S](val strength:S) extends Socket[Unit,S] {

  val space:SingleSpace[S] = I --> ScalarSpace(strength)

  val node:Node = new Node {
    val sockets = Set(Objective.this)
    val pins = Set()
    def eval(environment: VectorEnvironment):Unit = {}
    def rowWise[I](shape:Shape[I],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[I,_,_]]): Unit = {
      matrixifiedPins(pin.get).sum -> new Objective[S](strength)
    }
  }

  def evaluateFeedback(environment: VectorEnvironment): Unit = {
    environment.putFeedback(pin.get,new Single(strength))
  }

  def feedbackDependencies = Set()

}
