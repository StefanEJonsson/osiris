// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.vector

import osiris._
import osiris.vector.space.{PairSpace, VectorSpace}

trait Pair[L,R,S] extends container.Pair[L,R,S] with Vector[Either[L,R],S] {

  override val space:PairSpace[L,R,S]

  def left:Vector[L,S]
  def right:Vector[R,S]

  def bimap[L2,R2](l:Vector[L,S]=>Vector[L2,S],r:Vector[R,S]=>Vector[R2,S]):Pair[L2,R2,S] = Pair(l(left),r(right))

  def lmap[L2](l:Vector[L,S]=>Vector[L2,S]):Pair[L2,R,S] = Pair(l(left),right)

  def rmap[R2](r:Vector[R,S]=>Vector[R2,S]):Pair[L,R2,S] = Pair(left,r(right))

  override def swap:Pair[R,L,S] =
    this.reIndex(morphism.sum.commute[R,L](space.right.shape,space.left.shape)).asPair

  override def asPair[L2,R2,E<:Either[L2,R2] with Either[L,R]]: Pair[L2,R2,S] = this.asInstanceOf[Pair[L2,R2,S]]

}

class SimplePair[L,R,S] (val left:Vector[L,S], val right:Vector[R,S])
  extends Pair[L,R,S] {

  val space = left.space + right.space

  def apply(index:Either[L,R]):S = index match {
    case Left(i) => left(i)
    case Right(i) => right(i)
  }

  override def swap:Pair[R,L,S] = Pair(right,left)

  def serialize: Iterable[Byte] =
    Iterable(utilities.serialization.v2.Vector.pair) ++ left.serialize ++ right.serialize

}

object Pair {

  def apply[L,R,S](left:Vector[L,S],right:Vector[R,S]):Pair[L,R,S] = new SimplePair(left,right)

}