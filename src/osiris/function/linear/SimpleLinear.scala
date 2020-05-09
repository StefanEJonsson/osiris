// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear

import osiris.utilities.serialization.v2
import osiris.vector.{Matrix, Vector}

/**
  * A linear transformation implemented with a matrix k.
  */
class SimpleLinear[I,J,S](val k:Matrix[I,J,S]) extends LinearFunction[I,J,S] {

  val domain = k.inner
  val target = k.outer

  override def toString():String = s"Linear:\n$k"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.simpleLinear) ++ k.space.shape.serialize ++ k.serialize

  def apply(x:Vector[J,S]):Vector[I,S] = k*x

  def linearFeedback = new SimpleLinear(k.transpose)

  def <<[J0](that:SimpleLinear[J,J0,S]):SimpleLinear[I,J0,S] = new SimpleLinear(this.k * that.k)

}
