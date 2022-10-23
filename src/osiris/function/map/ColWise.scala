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
  * Takes a pair of matrices and produces a new matrix by applying the binary operation f on matching columns of the
  * matrices.
  *
  * z(j) = f(x(j),y(j))
  *
  * where z(j) denotes the jth column of the matrix z.
  */
class ColWise[I,IL,IR,J,S](inner:VectorSpace[J,S],f:VectorFunction[I,Either[IL,IR],S])
  extends VectorFunction[(I,J),Either[(IL,J),(IR,J)],S] {

  private val l = f.domain.asPairSpace[IL,IR,Either[IL,IR]].left
  private val r = f.domain.asPairSpace[IL,IR,Either[IL,IR]].right

  val target = f.target * inner
  val left =  l * inner
  val right = r * inner
  val domain = left + right

  override def toString():String = s"ColWise[$inner]($f)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.colWise) ++
    inner.shape.serialize ++ f.serialize

  def apply(x:Vector[Either[(IL,J),(IR,J)],S]):Vector[(I,J),S] = {
    val xp = x.asPair[(IL,J),(IR,J),Either[(IL,J),(IR,J)]]
    val xl = xp.left.asMatrix[IL,J,(IL,J)]
    val xr = xp.right.asMatrix[IR,J,(IR,J)]
    xl.colWise(f.target,(x:Vector[IL,S],y:Vector[IR,S]) => f(Pair(x,y)))(xr)
  }

  def feedback:VectorFunction[+[(IL,J),(IR,J)],+[+[(IL,J),(IR,J)],(I,J)],S] = new Lambda(
    domain + target, x => {

      val xp = x.asPair[+[(IL,J),(IR,J)],(I,J),+[+[(IL,J),(IR,J)],(I,J)]]
      val input = xp.left.asPair[(IL,J),(IR,J),+[(IL,J),(IR,J)]]
      val left = input.left.asMatrix[IL,J,(IL,J)]
      val right = input.right.asMatrix[IR,J,(IR,J)]
      val feed = xp.right.asMatrix[I,J,(I,J)]

      val res = (f.domain * inner).cols((j:J) => f.feedback((left.col(j)| right.col(j)) | feed.col(j)))
      res.permute(product.rightExtract(left.space.outer.shape,right.space.outer.shape,inner.shape))
    }
  )

}
