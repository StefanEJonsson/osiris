// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.map

import osiris.+
import osiris.function.{Lambda, VectorFunction}
import osiris.utilities.serialization.v2
import osiris.vector.Vector
import osiris.vector.space.VectorSpace

/**
  * Takes a matrix and produces a new matrix by applying the function f to all the columns.
  */
class ColMap[I1,I2,J,S](inner:VectorSpace[J,S],f:VectorFunction[I2,I1,S])
  extends VectorFunction[(I2,J),(I1,J),S] {

  val domain = f.domain * inner
  val target = f.target * inner

  override def toString():String = s"ColMap[$inner]($f)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.colMap) ++
    inner.shape.serialize ++ f.serialize

  def apply(x:Vector[(I1,J),S]):Vector[(I2,J),S] = x.asMatrix.colMap(f.target,f)

  def feedback:VectorFunction[(I1,J),+[(I1,J),(I2,J)],S] = new ColWise(inner,f.feedback)

}
