// SPDX-License-Identifier: GPL-2.1
//Copyright © 2020 Stefán E. Jónsson

package osiris.evaluator.machine

import osiris.pin.Pin
import osiris.pin.node.Node

case class NodeInstruction(node:Node) extends Instruction {

  def lock = node

  val valueDependencies:Set[Either[Pin[_,_],Pin[_,_]]] = node.sockets.map(s => Left(s.pin.get))

  val computationDependencies:Set[Instruction] = node.sockets.map(s => new NodeInstruction(s.pin.get.node))

}
