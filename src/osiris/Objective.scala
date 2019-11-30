// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris

import osiris.pin.{MatrixPin, Pin, Socket}
import osiris.evaluator.Environment
import osiris.pin.node.Node
import osiris.shape.Shape
import osiris.vector._
import osiris.vector.space.SingleSpace

import scala.collection.mutable

class Objective[S](strength:S) extends Socket[Unit,S] {

  val space:SingleSpace[S] = I --> ScalarSpace(strength)

  val node:Node = new Node {
    val sockets = Set(Objective.this)
    val pins = Set()
    def eval(environment: Environment):Unit = {}
    def rowWise[I](shape:Shape[I],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[I,_,_]]): Unit = {
      matrixifiedPins(pin.get).sum -> new Objective[S](strength)
    }
  }

  def evaluateFeedback(environment: Environment): Unit = {
    environment.putFeedback(pin.get,new Single(strength))
  }

  def feedbackDependencies = Set()

}
