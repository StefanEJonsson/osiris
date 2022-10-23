// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.map

import osiris.+
import osiris.function.VectorFunction
import osiris.utilities.serialization.v2
import osiris.vector.Vector
import osiris.vector.space.VectorSpace

/**
  * Takes a matrix and produces a new matrix by applying the function f to all the rows.
  */
class RowMap[I,J1,J2,S](outer:VectorSpace[I,S],f:VectorFunction[J2,J1,S])
  extends VectorFunction[(I,J2),(I,J1),S] {

  val domain = outer * f.domain
  val target = outer * f.target

  override def toString():String = s"RowMap[$outer]($f)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.rowMap) ++
    outer.shape.serialize ++ f.serialize

  def apply(x:Vector[(I,J1),S]):Vector[(I,J2),S] = x.asMatrix.rowMap(f.target,f)

  def feedback:VectorFunction[(I,J1),+[(I,J1),(I,J2)],S] = new RowWise(outer,f.feedback)

}