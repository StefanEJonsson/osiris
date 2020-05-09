// SPDX-License-Identifier: GPL-2.1
//Copyright © 2020 Stefán E. Jónsson

package osiris.evaluator.machine

import osiris.pin.Pin

case class FinalizeFeedback[I,S](pin:Pin[I,S]) extends Instruction {

  def lock = pin

  val valueDependencies: Set[Either[Pin[_, _], Pin[_, _]]] = Set()

  val computationDependencies: Set[Instruction] = pin.sockets.map(FeedbackInstruction(_))

}
