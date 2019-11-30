// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.map

import osiris.function.VectorFunction
import osiris.morphism.product
import osiris.vector.{Pair, Vector}
import osiris.vector.space.VectorSpace

class ColWise[I,IL,IR,J,S](inner:VectorSpace[J,S],f:VectorFunction[I,Either[IL,IR],S])
  extends VectorFunction[(I,J),Either[(IL,J),(IR,J)],S] {

  private val l = f.domain.asPairSpace[IL,IR,Either[IL,IR]].left
  private val r = f.domain.asPairSpace[IL,IR,Either[IL,IR]].right

  val target = f.target * inner
  val left =  l * inner
  val right = r * inner
  val domain = left + right

  def apply(x:Vector[Either[(IL,J),(IR,J)],S]):Vector[(I,J),S] = {
    val xp = x.asPair[(IL,J),(IR,J),Either[(IL,J),(IR,J)]]
    val xl = xp.left.asMatrix[IL,J,(IL,J)]
    val xr = xp.right.asMatrix[IR,J,(IR,J)]
    xl.colWise(f.target,(x:Vector[IL,S],y:Vector[IR,S]) => f(Pair(x,y)))(xr)
  }

  def feedback(x:Vector[Either[(IL,J),(IR,J)],S],y:Vector[(I,J),S]):Vector[Either[(IL,J),(IR,J)],S] = {
    val xm = x.reIndex((l+r)*inner,product.rightDistr[IL,IR,J](l.shape,r.shape,inner.shape)).asMatrix[Either[IL,IR],J,(Either[IL,IR],J)]
    xm.colWise(l+r,
      (x:Vector[Either[IL,IR],S],y:Vector[I,S]) => f.feedback(x,y)
    )(y.asMatrix).reIndex(domain,product.rightExtract[IL,IR,J](l.shape,r.shape,inner.shape))
  }

}
