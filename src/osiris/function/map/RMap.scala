// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.map

import osiris.+
import osiris.function.{Lambda, VectorFunction}
import osiris.utilities.serialization.v2
import osiris.vector.{Pair, Vector}
import osiris.vector.space.VectorSpace

/**
  * Takes a pair and produces a new pair by applying the function right to the right component.
  *
  * RMap(l,f)(x,y) = (x,f(y))
  *
  * @param left vector space of the left component.
  * @param right function to be applied on the right component.
  *
  */
class RMap[L,R1,R2,S](left:VectorSpace[L,S],right:VectorFunction[R2,R1,S])
  extends VectorFunction[Either[L,R2],Either[L,R1],S] {

  val domain = left + right.domain
  val target = left + right.target

  override def toString():String = s"RMap[$left]($right)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.rMap) ++
      left.shape.serialize ++ right.serialize

  def apply(x:Vector[Either[L,R1],S]):Vector[Either[L,R2],S] = x.asPair.rmap(right)

  def feedback:VectorFunction[+[L,R1],+[+[L,R1],+[L,R2]],S] = new Lambda(
    domain + target, x => {
      val xp = x.asPair[+[L,R1],+[L,R2],+[+[L,R1],+[L,R2]]]
      val input = xp.left.asPair[L,R1,+[L,R1]]
      val feed = xp.right.asPair[L,R2,+[L,R2]]

      feed.left | right.feedback(input.right | feed.right)
    }
  )

}