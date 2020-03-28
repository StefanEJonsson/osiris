// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.map

import osiris.function.linear.LinearFunction
import osiris.utilities.serialization.v2
import osiris.vector.{Matrix, Vector}
import osiris.vector.space.VectorSpace

/**
  * Applies the linear function f on every row of a matrix.
  *
  * Note that this does the exact same thing as osiris.function.map.RowMap, but since f is required to be linear,
  * RowMappedLinear can also extend LinearFunction.
  *
  * @param outer the vector space that columns of the matrix belong to.
  * @param f the function to applied on the rows.
  * @tparam I the index type for the columns
  * @tparam J1 the index type for the rows of the input matrix.
  * @tparam J2 the index type for the rows of the output matrix.
  * @tparam S the scalar type.
  */
class RowMappedLinear[I,J1,J2,S](outer:VectorSpace[I,S], f:LinearFunction[J2,J1,S])
  extends LinearFunction[(I,J2),(I,J1),S] {

  val domain = outer * f.domain
  val target = outer * f.target

  override def toString():String = s"RowMap[$outer]($f)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.rowMappedLinear) ++
      outer.shape.serialize ++ f.serialize

  def apply(x:Vector[(I,J1),S]):Matrix[I,J2,S] = x.asMatrix.rowMap(f.target,f)

  def feedback = new RowMappedLinear(outer,f.feedback)

}