// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris

import osiris.pin.Pin

package object evaluator {

  def remove(pin:Pin[_,_]): Unit = {
    val toBeRemoved = collection.mutable.Queue[Pin[_,_]](pin)
    while (toBeRemoved.nonEmpty) {
      val pi = toBeRemoved.dequeue()
      type I = pi.space.shape.Type
      type S = pi.space.scalarSpace.Type
      val p = pi.asInstanceOf[Pin[I,S]]
      for (s <- p.secondary) {toBeRemoved.enqueue(s)}
      for (s <- p.sockets) {
        p -/- s
      }
      if (p.node.pins.forall(_.sockets.isEmpty)) {
        p.node.sockets.foreach(_.pin.foreach(toBeRemoved.enqueue(_)))
      }
    }
  }

}
