// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin

import osiris._
import function.{ScalarFunction, VectorFunction}
import pin.node.split.PairSplit
import osiris.vector.{Vector,Pair}
import osiris.vector.space.PairSpace

trait PairPin[L,R,S] extends Pin[Either[L,R],S] {

  val space:PairSpace[L,R,S]

  private var split = Option.empty[PairSplit[L,R,S]]

  private def createSplit(): Unit = {
    split = Option(new PairSplit(space.left,space.right))
    this ->- split.get.in
  }

  def left:Pin[L,S] = {
    if(split.isEmpty) {
      createSplit()
    }
    split.get.left
  }

  def right:Pin[R,S] = {
    if(split.isEmpty) {
      createSplit()
    }
    split.get.right
  }

  override def map(f:ScalarFunction[S]):PairPin[L,R,S] = super.map(f).asPair

  override def map(f:SinglePin[S] => Pin[Unit,S]):PairPin[L,R,S] = super.map(f).asPair

  def lmap[L2](f:Pin[L,S] => Pin[L2,S]):PairPin[L2,R,S] = f(left) | right

  def rmap[R2](f:Pin[R,S] => Pin[R2,S]):PairPin[L,R2,S] = left | f(right)

  def bimap[L2,R2](l:Pin[L,S]=>Pin[L2,S],r:Pin[R,S]=>Pin[R2,S]):PairPin[L2,R2,S] = l(left) | r(right)

  override def elemWise(op:VectorFunction[Unit,Either[Unit,Unit],S])(that:Pin[Either[L,R],S]):PairPin[L,R,S] =
    super.elemWise(op)(that).asPair

  override def elemWise(op:(SinglePin[S],SinglePin[S]) => Pin[Unit,S])(that:Pin[Either[L,R],S]):PairPin[L,R,S] =
    (super.elemWise(op)(that)).asPair

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def *(k:SinglePin[S]):PairPin[L,R,S] = super.*(k).asPair

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def +(that:Pin[Either[L,R],S]):PairPin[L,R,S] = super.+(that).asPair

  override def -(that:Pin[Either[L,R],S]):PairPin[L,R,S] = super.-(that).asPair

  override def unary_-():PairPin[L,R,S] = super.unary_-().asPair

  override def o(that:Pin[Either[L,R],S]):PairPin[L,R,S] = super.o(that).asPair

}
