// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.bilinear

import osiris.vector.space.VectorSpace
import osiris.vector.{Matrix, Vector}

case class ElementWiseMultiplication[I,S](target:VectorSpace[I,S]) extends BilinearFunction[I,I,I,S] {

  val left = target
  val right = target

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
