// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.vector

import osiris._
import osiris.vector.space.SingleSpace

class Single[S](value:S) extends container.Single[S](value) with Vector[Unit,S] {

  override val space:SingleSpace[S] = I --> ScalarSpace(value)

  override def +(that:Vector[Unit,S]):Single[S] = super.+(that).asSingle

  override def -(that:Vector[Unit,S]):Single[S] = super.-(that).asSingle

  override def o(that:Vector[Unit,S]):Single[S] = super.o(that).asSingle

  def /(that:Single[S]):Single[S] = new Single(space.scalarSpace./(this.value,that.value))

  def inv:Single[S] = new Single(space.scalarSpace.inv(value))

  def ^(that:Single[S]):Single[S] = new Single(space.scalarSpace.^(this.value,that.value))

  def ^(k:Double):Single[S] = new Single(space.scalarSpace.^(this.value,space.scalarSpace.fromDouble(k))) //TODO make sure that corresponding function exists in scalarspace specification

  def serialize: Iterable[Byte] =
    Iterable(utilities.serialization.v2.Vector.elems) ++ space.scalarSpace.serialize(value)

}