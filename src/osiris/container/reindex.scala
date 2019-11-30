// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.container

import osiris._
import osiris.morphism._
import container.companion.ContainerCompanion
import osiris.shape.Shape

class ReIndexed[I,J,S](v:Container[J,S], converter:Morphism[I,J])
  extends Container[I,S] {

  val space = converter.domain -->[S]()

  def apply(i:I) = v(converter(i))

  override def reIndex[I2](newConverter:Morphism[I2,I]):Container[I2,S] =
    new ReIndexed(v,converter << newConverter)

  override def asEmpty[N<:Nothing with I]:Empty[S] = new ReIndexedEmpty(this.asInstanceOf[ReIndexed[Nothing,J,S]])

  override def asSingle[U<:Unit with I]:Single[S] = new ReIndexedSingle(this.asInstanceOf[ReIndexed[Unit,J,S]])

  override def asSequential[int<:Int with I]:Sequential[S] =
    new ReIndexedSequential(this.asInstanceOf[ReIndexed[Int,J,S]])

  override def asPair[L,R,sum<:Either[L,R] with I]:Pair[L,R,S] =
    new ReIndexedPair(this.asInstanceOf[ReIndexed[Either[L,R],J,S]])

  override def asTable[Out,In,prod<:(Out,In) with I]:Table[Out,In,S] =
    new ReIndexedTable(this.asInstanceOf[ReIndexed[(Out,In),J,S]])

}


class ReIndexedEmpty[J,S](original:ReIndexed[Nothing,J,S]) extends Empty[S]

class ReIndexedSingle[J,S](original:ReIndexed[Unit,J,S]) extends Single[S](original(()))

class ReIndexedSequential[J,S](original:ReIndexed[Int,J,S]) extends Sequential[S] {

  val space = original.space.asSequential

  def apply(i:Int) = original(i)

}


class ReIndexedPair[L,R,J,S](original:ReIndexed[Either[L,R],J,S])
  extends Pair[L,R,S] {

  val space = original.space.asPair

  lazy val left  = this.reIndex(morphism.sum.left(space.left.shape,space.right.shape))
  lazy val right = this.reIndex(morphism.sum.right(space.left.shape,space.right.shape))

  def apply(i:Either[L,R]) = original(i)

}

class ReIndexedTable[R,C,J,S](original:ReIndexed[(R,C),J,S]) extends Table[R,C,S] {

  val outer = original.space.asTable.outer
  val inner = original.space.asTable.inner

  def apply(i:(R,C)) = original(i)

}

