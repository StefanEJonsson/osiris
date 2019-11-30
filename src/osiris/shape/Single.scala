// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.shape

import osiris.{ScalarSpace, container, utilities}
import osiris.vector.space.SingleSpace

object Single extends Shape[Unit] {

  override def size = 1

  def serialize:Iterable[Byte] = Iterable(utilities.Serialization.Shape.single)

  def deserializeIndex(bytes: Iterator[Byte]): Unit = ()

  def iterator = Seq(()).iterator

  def -->[S]():container.companion.SingleCompanion[S] = new container.companion.SingleCompanion[S]

  def -->[S](s:ScalarSpace[S]):SingleSpace[S] = new SingleSpace(s)

  override def toString: String = "I"

}
