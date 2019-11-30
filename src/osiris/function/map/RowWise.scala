// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.map

import osiris.function.VectorFunction
import osiris.morphism.product
import osiris.vector.{Pair, Vector}
import osiris.vector.space.VectorSpace

class RowWise[I,J,JL,JR,S](outer:VectorSpace[I,S],f:VectorFunction[J,Either[JL,JR],S])
  extends VectorFunction[(I,J),Either[(I,JL),(I,JR)],S] {

  private val l = f.domain.asPairSpace[JL,JR,Either[JL,JR]].left
  private val r = f.domain.asPairSpace[JL,JR,Either[JL,JR]].right

  val target = outer * f.target
  val left = outer * l
  val right = outer * r
  val domain = left + right

  def apply(x:Vector[Either[(I,JL),(I,JR)],S]):Vector[(I,J),S] = {
    val xp = x.asPair[(I,JL),(I,JR),Either[(I,JL),(I,JR)]]
    val xl = xp.left.asMatrix[I,JL,(I,JL)]
    val xr = xp.right.asMatrix[I,JR,(I,JR)]
    xl.rowWise(f.target,(x:Vector[JL,S],y:Vector[JR,S]) => f(Pair(x,y)))(xr)
  }

  def feedback(x:Vector[Either[(I,JL),(I,JR)],S],y:Vector[(I,J),S]):Vector[Either[(I,JL),(I,JR)],S] = {
    val xm = x.reIndex(outer*(l+r),product.leftDistr[I,JL,JR](outer.shape,l.shape,r.shape)).asMatrix[I,Either[JL,JR],(I,Either[JL,JR])]
    xm.rowWise(l+r,(x:Vector[Either[JL,JR],S],y:Vector[J,S]) =>
      f.feedback(x,y)
    )(y.asMatrix).reIndex(domain,product.leftExtract[I,JL,JR](outer.shape,l.shape,r.shape))
  }

}