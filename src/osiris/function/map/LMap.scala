// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.map

import osiris.function.VectorFunction
import osiris.vector.{Pair, Vector}
import osiris.vector.space.VectorSpace

class LMap[L1,L2,R,S](left:VectorFunction[L2,L1,S],right:VectorSpace[R,S])
  extends VectorFunction[Either[L2,R],Either[L1,R],S] {

  val domain = left.domain + right
  val target = left.target + right

  def apply(x:Vector[Either[L1,R],S]):Vector[Either[L2,R],S] = x.asPair.lmap(left)

  def feedback(x:Vector[Either[L1,R],S],y:Vector[Either[L2,R],S]):Vector[Either[L1,R],S] = {
    val xp = x.asPair[L1,R,Either[L1,R]]
    val yp = y.asPair[L2,R,Either[L2,R]]
    Pair(left.feedback(xp.left,yp.left),yp.right)
  }

}
