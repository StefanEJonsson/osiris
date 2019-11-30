// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node

import osiris.evaluator.Environment
import osiris.pin.{MatrixPin, Pin, Socket}
import osiris.shape.Shape

trait Node {

  val sockets:Set[Socket[_,_]]
  val pins:Set[Pin[_,_]]

  def eval(environment:Environment)

  def rowWise[I](shape:Shape[I],matrixifiedPins:collection.mutable.Map[Pin[_,_],MatrixPin[I,_,_]])

}
