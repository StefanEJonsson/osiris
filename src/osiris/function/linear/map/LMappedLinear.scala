// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.map

import osiris.function.linear.LinearFunction
import osiris.vector.{Pair, Vector}
import osiris.vector.space.VectorSpace

class LMappedLinear[L1,L2,R,S](l:LinearFunction[L2,L1,S],r:VectorSpace[R,S])
  extends LinearFunction[Either[L2,R],Either[L1,R],S] {


  val domain = l.domain + r
  val target = l.target + r

  def apply(x:Vector[Either[L1,R],S]):Pair[L2,R,S] = x.asPair.lmap(l)

  def feedback = new LMappedLinear(l.feedback,r)


}
