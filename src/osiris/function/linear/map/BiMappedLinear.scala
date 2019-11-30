// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.map

import osiris.function.linear.LinearFunction
import osiris.vector.{Pair, Vector}

class BiMappedLinear[L1,L2,R1,R2,S](l:LinearFunction[L2,L1,S],r:LinearFunction[R2,R1,S])
  extends LinearFunction[Either[L2,R2],Either[L1,R1],S] {

  val domain = l.domain + r.domain
  val target = l.target + r.target

  def apply(x:Vector[Either[L1,R1],S]):Pair[L2,R2,S] = x.asPair.bimap(l,r)

  def feedback = new BiMappedLinear(l.feedback,r.feedback)

}