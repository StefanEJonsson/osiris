// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.map

import osiris.+
import osiris.function.{Lambda, VectorFunction}
import osiris.morphism.product
import osiris.utilities.serialization.v2
import osiris.vector.{Pair, Vector}
import osiris.vector.space.VectorSpace

/**
  * Takes a pair of matrices and produces a new matrix by applying the binary operation f on matching rows of the
  * matrices.
  *
  * z(i) = f(x(i),y(i))
  *
  * where z(i) denotes the ith row of the matrix z.
  */
class RowWise[I,J,JL,JR,S](outer:VectorSpace[I,S],f:VectorFunction[J,Either[JL,JR],S])
  extends VectorFunction[(I,J),Either[(I,JL),(I,JR)],S] {

  private val l = f.domain.asPairSpace[JL,JR,Either[JL,JR]].left
  private val r = f.domain.asPairSpace[JL,JR,Either[JL,JR]].right

  val target = outer * f.target
  val left = outer * l
  val right = outer * r
  val domain = left + right

  override def toString():String = s"RowWise[$outer]($f)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.rowWise) ++
    outer.shape.serialize ++ f.serialize

  def apply(x:Vector[Either[(I,JL),(I,JR)],S]):Vector[(I,J),S] = {
    val xp = x.asPair[(I,JL),(I,JR),Either[(I,JL),(I,JR)]]
    val xl = xp.left.asMatrix[I,JL,(I,JL)]
    val xr = xp.right.asMatrix[I,JR,(I,JR)]
    xl.rowWise(f.target,(x:Vector[JL,S],y:Vector[JR,S]) => f(Pair(x,y)))(xr)
  }

  def feedback:VectorFunction[+[(I,JL),(I,JR)],+[+[(I,JL),(I,JR)],(I,J)],S] = new Lambda(
    domain + target, x => {

      val xp = x.asPair[+[(I,JL),(I,JR)],(I,J),+[+[(I,JL),(I,JR)],(I,J)]]
      val input = xp.left.asPair[(I,JL),(I,JR),+[(I,JL),(I,JR)]]
      val left = input.left.asMatrix[I,JL,(I,JL)]
      val right = input.right.asMatrix[I,JR,(I,JR)]
      val feed = xp.right.asMatrix[I,J,(I,J)]

      val res = (outer * f.domain).rows((i:I) => f.feedback((left.row(i)| right.row(i)) | feed.row(i)))
      res.permute(product.leftExtract(outer.shape,left.space.inner.shape,right.space.inner.shape))
    }
  )

}