// SPDX-License-Identifier: GPL-2.1
//Copyright © 2020 Stefán E. Jónsson

package osiris.evaluator.machine

import osiris.pin.Pin

trait Instruction {

  def lock:Object

  val valueDependencies:Set[Either[Pin[_,_],Pin[_,_]]]

  val computationDependencies:Set[Instruction]

}
