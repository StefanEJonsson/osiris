// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.map

import osiris.function.linear.LinearFunction
import osiris.utilities.serialization.v2
import osiris.vector.{Matrix, Vector}
import osiris.vector.space.VectorSpace

/**
  * Applies the linear function f on every column of a matrix.
  *
  * Note that this does the exact same thing as osiris.function.map.ColMap, but since f is required to be linear,
  * ColMappedLinear can also extend LinearFunction.
  *
  * @param inner the vector space that rows of the matrix belong to.
  * @param f the function to be applied on the columns of the matrix.
  * @tparam I1 the index type for columns of the input matrix.
  * @tparam I2 the index type for columns of the output matrix.
  * @tparam J the index type for rows of the matrix.
  * @tparam S the scalar type.
  */
class ColMappedLinear[I1,I2,J,S](inner:VectorSpace[J,S], f:LinearFunction[I2,I1,S])
  extends LinearFunction[(I2,J),(I1,J),S] {

  val domain = f.domain * inner
  val target = f.target * inner

  override def toString():String = s"ColMap[$inner]($f)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.colMappedLinear) ++
      inner.shape.serialize ++ f.serialize

  def apply(x:Vector[(I1,J),S]):Matrix[I2,J,S] = x.asMatrix.colMap(f.target,f)

  def feedback = new ColMappedLinear(inner,f.feedback)

}
