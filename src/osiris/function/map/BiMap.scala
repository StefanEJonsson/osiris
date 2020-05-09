// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.map

import osiris.+
import osiris.function.{Lambda, VectorFunction}
import osiris.utilities.serialization.v2
import osiris.vector.{Pair, Vector}

/**
  * Takes a pair and produces a new pair by applying the function left to the left component and the function right to
  * the right component.
  */
class BiMap[IL,IR,JL,JR,S](left:VectorFunction[IL,JL,S], right:VectorFunction[IR,JR,S])
  extends VectorFunction[Either[IL,IR],Either[JL,JR],S] {

  val domain = left.domain + right.domain
  val target = left.target + right.target

  override def toString():String = s"BiMap[$left $right]"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.biMap) ++
    left.serialize ++ right.serialize

  def apply(x:Vector[Either[JL,JR],S]):Vector[Either[IL,IR],S] = {
    val xp = x.asPair[JL,JR,Either[JL,JR]]
    left(xp.left) | right(xp.right)
  }

  def feedback:VectorFunction[+[JL,JR],+[+[JL,JR],+[IL,IR]],S] = new Lambda(
    domain + target, x => {
      val xp = x.asPair[+[JL,JR],+[IL,IR],+[+[JL,JR],+[IL,IR]]]
      val input = xp.left.asPair[JL,JR,+[JL,JR]]
      val feed = xp.right.asPair[IL,IR,+[IL,IR]]
      left.feedback(input.left | feed.left) | right.feedback(input.right | feed.right)
    }
  )

}