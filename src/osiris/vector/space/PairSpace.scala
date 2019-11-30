// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.vector.space

import osiris._
import morphism.Morphism
import utilities.same
import vector.{Empty, Pair}

class PairSpace[L,R,S] (override val left:VectorSpace[L,S],override val right:VectorSpace[R,S])
  extends container.companion.PairCompanion[L,R,S](left,right) with VectorSpace[Either[L,R],S] {

  val scalarSpace = same(left.scalarSpace,right.scalarSpace)

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def swap:PairSpace[R,L,S] = new PairSpace(right,left)

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def open(file:String):Pair[L,R,S] = super.open(file).asPair

  override def apply(f:Either[L,R]=>S):Pair[L,R,S] = Pair(left( l => f(Left(l))),right( r => f(Right(r))))

  override def apply(f:Either[L,R] => pin.Pin[Unit,S]):pin.PairPin[L,R,S] = super.apply(f).asPair

  private[space] def parseElems(bytes: Iterator[Byte]): Pair[L,R,S] = {
    val l = left.parseElems(bytes)
    val r = right.parseElems(bytes)
    Pair[L,R,S](l,r)
  }

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def fill(value:S):Pair[L,R,S] = super.fill(value).asPair

  override def zeros:Pair[L,R,S] = super.zeros.asPair

  override def ones:Pair[L,R,S] = super.ones.asPair

  override def unit(index:Either[L,R]):Pair[L,R,S] = super.unit(index).asPair

  override def units(predicate:Morphism[Either[L,R],Either[Unit,Unit]]):Pair[L,R,S] = super.units(predicate).asPair

  /* ---------------------------------------------------------------------------------------------------------------- */

}
