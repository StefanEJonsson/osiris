// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.map

import osiris.function.{Lambda, VectorFunction}
import osiris.{+, utilities}
import osiris.utilities.serialization.v2
import osiris.vector.{Pair, Vector}
import osiris.vector.space.VectorSpace

/**
  * Takes a pair and produces a new pair by applying the function left to the left component.
  *
  * LMap(f,r)(x,y) = (f(x),y)
  *
  * @param left function to be applied on left component.
  * @param right vector space of the right component.
  *
  */
class LMap[L1,L2,R,S](left:VectorFunction[L2,L1,S],right:VectorSpace[R,S])
  extends VectorFunction[Either[L2,R],Either[L1,R],S] {

  val domain = left.domain + right
  val target = left.target + right

  override def toString():String = s"LMap[$right]($left)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.lMap) ++
    left.serialize ++ right.shape.serialize

  def apply(x:Vector[Either[L1,R],S]):Vector[Either[L2,R],S] = x.asPair.lmap(left)

  def feedback:VectorFunction[+[L1,R],+[+[L1,R],+[L2,R]],S] = new Lambda(
    domain + target, x => {
      val xp = x.asPair[+[L1,R],+[L2,R],+[+[L1,R],+[L2,R]]]
      val input = xp.left.asPair[L1,R,+[L1,R]]
      val feed = xp.right.asPair[L2,R,+[L2,R]]

      left.feedback(input.left | feed.left) | feed.right
    }
  )

}
