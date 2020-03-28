// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.shape

import osiris.utilities.serialization.v2
import osiris.{ScalarSpace, container}
import osiris.vector.space.EmptySpace

/**
  * The shape with no elements (the empty set).
  */
object Empty extends Shape[Nothing] {

  override def size = 0

  def serialize:Iterable[Byte] = Iterable(v2.Shape.empty)

  def deserializeIndex(bytes: Iterator[Byte]): Nothing = throw new Exception("No such thing exists.")

  def iterator = Iterator.empty

  def -->[S]():container.companion.EmptyCompanion[S] = new container.companion.EmptyCompanion[S]

  def -->[S](s:ScalarSpace[S]):EmptySpace[S] = new EmptySpace(s)

  override def toString: String = "O"

}
