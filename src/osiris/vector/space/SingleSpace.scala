// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.vector.space

import osiris._
import morphism.Morphism
import vector.Single

class SingleSpace[S] (val scalarSpace:ScalarSpace[S])
  extends container.companion.SingleCompanion[S] with VectorSpace[Unit,S] {

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def open(file:String):Single[S] = super.open(file).asSingle

  override def apply(f:Unit => S):Single[S] = new Single(f())

  override def apply(f:Unit => pin.Pin[Unit,S]):pin.SinglePin[S] = super.apply(f).asSingle

  private[space] def parseElems(bytes:Iterator[Byte]):Single[S] = this(_ => scalarSpace.deserialize(bytes))

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def fill(value:S):Single[S] = new Single(value)

  override def zeros:Single[S] = new Single(scalarSpace.zero)

  override def ones:Single[S] = new Single(scalarSpace.one)

  override def unit(index:Unit):Single[S] = new Single(scalarSpace.one)

  override def units(predicate:Morphism[Unit,Either[Unit,Unit]]):Single[S] = predicate(()) match {
    case Left(()) => new Single(scalarSpace.zero)
    case Right(()) => new Single(scalarSpace.one)
  }

  /* ---------------------------------------------------------------------------------------------------------------- */

}