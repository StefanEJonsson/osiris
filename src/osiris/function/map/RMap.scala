// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.map

import osiris.function.VectorFunction
import osiris.vector.{Pair, Vector}
import osiris.vector.space.VectorSpace

class RMap[L,R1,R2,S](left:VectorSpace[L,S],right:VectorFunction[R2,R1,S])
  extends VectorFunction[Either[L,R2],Either[L,R1],S] {

  val domain = left + right.domain
  val target = left + right.target

  def apply(x:Vector[Either[L,R1],S]):Vector[Either[L,R2],S] = x.asPair.rmap(right)

  def feedback(x:Vector[Either[L,R1],S],y:Vector[Either[L,R2],S]):Vector[Either[L,R1],S] = {
    val xp = x.asPair[L,R1,Either[L,R1]]
    val yp = y.asPair[L,R2,Either[L,R2]]
    Pair(yp.left,right.feedback(xp.right,yp.right))
  }

}