// SPDX-License-Identifier: GPL-2.1
//Copyright © 2020 Stefán E. Jónsson

package osiris.evaluator.machine

import osiris.pin.{Pin, Socket}

case class FeedbackInstruction(socket:Socket[_,_]) extends Instruction {

  def lock = socket

  val valueDependencies = socket.feedbackDependencies

  val computationDependencies:Set[Instruction] =
    socket.feedbackDependencies.map {
      d => d match {
        case Left(pin) => NodeInstruction(pin.node)
        case Right(pin) => FinalizeFeedback(pin)
      }
    }

}
