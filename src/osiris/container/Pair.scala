// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.container

import osiris._
import osiris.container.companion.PairCompanion

/**
  * A container consisting of two smaller containers concatenated together to form a pair of containers.
  *
  * The index type of this container is Either[L,R]. Its shape is the tagged union of the shapes of the left and right
  * containers respectively.
  *
  * @tparam L the index type for the left container
  * @tparam R the index type for the right container
  * @tparam S the type of elements stored in the container.
  */
trait Pair[L,R,S] extends Container[Either[L,R],S] {

  val space:PairCompanion[L,R,S]

  /**
    * @return The first (left) component of this pair.
    */
  def left:Container[L,S]
  /**
    * @return The second (right) component of this pair.
    */
  def right:Container[R,S]

  override def toString():String = s"($left|$right)"

  def bimap[L2,R2](l:Container[L,S]=>Container[L2,S], r:Container[R,S]=>Container[R2,S]):Pair[L2,R2,S] = Pair(l(left),r(right))

  def lmap[L2](l:Container[L,S]=>Container[L2,S]):Pair[L2,R,S] = Pair(l(left),right)

  def rmap[R2](r:Container[R,S]=>Container[R2,S]):Pair[L,R2,S] = Pair(left,r(right))

  def swap:Pair[R,L,S] = this.reIndex(morphism.sum.commute(space.right.shape,space.left.shape)).asPair

}

class SimplePair[L,R,S] (val left:Container[L,S], val right:Container[R,S])
  extends Pair[L,R,S] {

  val space = left.space + right.space

  def apply(index:Either[L,R]):S = index match {
    case Left(i) => left(i)
    case Right(i) => right(i)
  }

  override def swap:Pair[R,L,S] = Pair(right,left)

}

object Pair {

  def apply[L,R,S](left:Container[L,S], right:Container[R,S]):Pair[L,R,S] =
    new SimplePair(left,right)

}