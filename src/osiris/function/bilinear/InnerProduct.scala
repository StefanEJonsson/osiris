// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.bilinear

import osiris.utilities.serialization.v2
import osiris.vector.space.VectorSpace
import osiris.vector.{Matrix, Single, Vector}
import osiris._

/**
  * z = sum(x(i)*y(i))
  */
case class InnerProduct[I,S](left:VectorSpace[I,S]) extends BilinearFunction[Unit,I,I,S] {

  val right = left
  val target = I --> left.scalarSpace

  override def toString():String = s"<>[$left]"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.innerProduct) ++
    left.shape.serialize

  override def apply(x:Vector[Either[I,I],S]):Vector[Unit,S] = {
    val xp = x.asPair[I,I,Either[I,I]]
    new Single(xp.left <> xp.right)
  }

  def apply[II,LL,RR](iSpace:VectorSpace[II,S],lSpace:VectorSpace[LL,S],rSpace:VectorSpace[RR,S],
                      l:Matrix[I,LL,S],r:Matrix[I,RR,S],
                      op:(Vector[LL,S],Vector[RR,S])=>Vector[II,S]):Matrix[Unit,II,S] =
    (l.rowWise(iSpace,op)(r)).colSum.toRowVector

  def leftFeedback = LeftScalarProduct(left)
  def rightFeedback = LeftScalarProduct(left)

}

object InnerProduct {

  def apply[I,S](space:VectorSpace[I,S]):InnerProduct[I,S] = new InnerProduct(space)

}