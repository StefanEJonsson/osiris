// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node

import osiris.evaluator.environment.VectorEnvironment
import osiris.pin.{MatrixPin, Pin, Socket}
import osiris.shape.Shape

/**
  * A node represents a computation in a computation graph. The node has a set of sockets and a set of pins. The sockets
  * represent inputs to the computation and pins represent outputs.
  */
trait Node {

  val sockets:Set[Socket[_,_]]
  val pins:Set[Pin[_,_]]

  def eval(environment:VectorEnvironment)

  def rowWise[I](shape:Shape[I],matrixifiedPins:collection.mutable.Map[Pin[_,_],MatrixPin[I,_,_]])

}
