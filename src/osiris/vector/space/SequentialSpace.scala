// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.vector.space

import osiris._
import shape._
import vector.{Sequential, SimpleSequential}
import morphism._

class SequentialSpace[S] (val scalarSpace:ScalarSpace[S], range:Range)
  extends container.companion.SequentialCompanion[S](range) with VectorSpace[Int,S] {

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def *[J](that:VectorSpace[J,S]) = shape --> that

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def open(file:String):Sequential[S] = super.open(file).asSequential

  override def apply(f:Int => S):Sequential[S] = new SimpleSequential(this,shape.map(f).toVector)

  override def apply(f:Int => pin.Pin[Unit,S]):pin.SequentialPin[S] = super.apply(f).asSequential

  private[space] def parseElems(bytes:Iterator[Byte]):Sequential[S] =
    new SimpleSequential(this,range.map(_ => scalarSpace.deserialize(bytes)).toVector)

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def fill(value:S):Sequential[S] = super.fill(value).asSequential

  override def zeros:Sequential[S] = fill(scalarSpace.zero)

  override def ones:Sequential[S] = fill(scalarSpace.one)

  override def unit(index:Int):Sequential[S] = super.unit(index).asSequential

  override def units(predicate:Morphism[Int,Either[Unit,Unit]]):Sequential[S] = super.units(predicate).asSequential

  /* ---------------------------------------------------------------------------------------------------------------- */

}