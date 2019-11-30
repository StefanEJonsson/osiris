// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.map

import osiris.function.VectorFunction
import osiris.vector.{Pair, Vector}

class BiMap[IL,IR,JL,JR,S](left:VectorFunction[IL,JL,S], right:VectorFunction[IR,JR,S])
  extends VectorFunction[Either[IL,IR],Either[JL,JR],S] {

  val domain = left.domain + right.domain
  val target = left.target + right.target

  override def toString():String = s"PairMap $left $right"

  def apply(x:Vector[Either[JL,JR],S]):Vector[Either[IL,IR],S] = {
    val xp = x.asPair[JL,JR,Either[JL,JR]]
    left(xp.left) | right(xp.right)
  }

  def feedback(x:Vector[Either[JL,JR],S],y:Vector[Either[IL,IR],S]):Vector[Either[JL,JR],S] = {
    val xp = x.asPair[JL,JR,Either[JL,JR]]
    val yp = y.asPair[IL,IR,Either[IL,IR]]
    Pair(left.feedback(xp.left,yp.left),right.feedback(xp.right,yp.right))
  }

}