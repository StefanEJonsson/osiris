// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.map

import osiris.function.linear.LinearFunction
import osiris.vector.{Pair, Vector}
import osiris.vector.space.VectorSpace

class RMappedLinear[L,R1,R2,S](l:VectorSpace[L,S],r:LinearFunction[R2,R1,S])
  extends LinearFunction[Either[L,R2],Either[L,R1],S] {

  val domain = l + r.domain
  val target = l + r.target

  def apply(x:Vector[Either[L,R1],S]):Pair[L,R2,S] = x.asPair.rmap(r)

  def feedback = new RMappedLinear(l,r.feedback)

}