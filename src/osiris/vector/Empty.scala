// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.vector

import osiris._

/**
  * The vector containing no elements.
  */
class Empty[S] (scalarSpace: ScalarSpace[S]) extends container.Empty[S] with Vector[Nothing,S] {

  override val space = O --> scalarSpace

  def serialize: Iterable[Byte] = Iterable(utilities.serialization.v2.Vector.elems)

  override def asEmpty[n <: Nothing with Nothing]: Empty[S] = this

}
