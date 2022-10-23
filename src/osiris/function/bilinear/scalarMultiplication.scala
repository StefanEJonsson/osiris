// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.bilinear

import osiris.utilities.serialization.v2
import osiris.vector.space.VectorSpace
import osiris.vector.{Matrix, Vector}
import osiris._

/**
  * Multiplication between scalar and vector (scalar on the left).
  */
case class LeftScalarProduct[I,S](target:VectorSpace[I,S]) extends BilinearFunction[I,Unit,I,S] {

  val left = I --> target.scalarSpace
  val right = target

  override def toString():String = s"<*[$target]"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.leftScalarProduct) ++
    target.shape.serialize

  override def apply(x:Vector[Either[Unit,I],S]):Vector[I,S] = {
    val xp = x.asPair[Unit,I,Either[Unit,I]]
    xp.right * xp.left.asSingle.value
  }

  def apply[II,LL,RR](iSpace:VectorSpace[II,S],lSpace:VectorSpace[LL,S],rSpace:VectorSpace[RR,S],
                      l:Matrix[Unit,LL,S],r:Matrix[I,RR,S],
                      op:(Vector[LL,S],Vector[RR,S]) => Vector[II,S]):Matrix[I,II,S] =
    r.rowMap(iSpace,r => op(l.row(()),r))

  def leftFeedback = InnerProduct(target)

  def rightFeedback = RightScalarProduct(target)

}

/**
  * Multiplication between vector and scalar (scalar on the right).
  */
case class RightScalarProduct[I,S](target:VectorSpace[I,S]) extends BilinearFunction[I,I,Unit,S] {

  val left = target
  val right = I --> target.scalarSpace

  override def toString():String = s">*[$target]"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.rightScalarProduct) ++
    target.shape.serialize

  override def apply(x:Vector[Either[I,Unit],S]):Vector[I,S] = {
    val xp = x.asPair[I,Unit,Either[I,Unit]]
    xp.left*xp.right.asSingle.value
  }

  def apply[II,LL,RR](iSpace:VectorSpace[II,S],lSpace:VectorSpace[LL,S],rSpace:VectorSpace[RR,S],
                      l:Matrix[I,LL,S],r:Matrix[Unit,RR,S],
                      op:(Vector[LL,S],Vector[RR,S]) => Vector[II,S]):Matrix[I,II,S] =
    l.rowMap(iSpace,lr => op(lr,r.row(())))

  def leftFeedback = RightScalarProduct(target)

  def rightFeedback = InnerProduct(target)

}