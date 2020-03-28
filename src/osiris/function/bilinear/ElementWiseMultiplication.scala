// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.bilinear

import osiris.utilities.serialization.v2
import osiris.vector.space.VectorSpace
import osiris.vector.{Matrix, Vector}

/**
  * z(i) = x(i)*y(i)
  */
case class ElementWiseMultiplication[I,S](target:VectorSpace[I,S]) extends BilinearFunction[I,I,I,S] {

  val left = target
  val right = target

  override def toString():String = s"o[$target]"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.elementWiseMultiplication) ++
    target.shape.serialize

  override def apply(x:Vector[Either[I,I],S]):Vector[I,S] = {
    val xp = x.asPair[I,I,Either[I,I]]
    xp.left o xp.right
  }

  def apply[II,LL,RR](iSpace:VectorSpace[II,S],lSpace:VectorSpace[LL,S],rSpace:VectorSpace[RR,S],
                      l:Matrix[I,LL,S],r:Matrix[I,RR,S],
                      op:(Vector[LL,S],Vector[RR,S]) => Vector[II,S]):Matrix[I,II,S] =
    l.rowWise(iSpace,op)(r)

  def leftFeedback = this

  def rightFeedback = this

}

object ElementWiseMultiplication {

  def apply[I,S](target:VectorSpace[I,S]):ElementWiseMultiplication[I,S] = new ElementWiseMultiplication(target)

}