// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function

import osiris._
import osiris.utilities.serialization.v2
import vector._

/**
  * Constant vector function.
  *
  * Its input is the empty vector (a vector containing no elements) and its output is always c.
  *
  * @param c the constant output of the function.
  */
class Constant[I,S](c:Vector[I,S]) extends VectorFunction[I,Nothing,S] {

  val target = c.space
  val domain = O --> target.scalarSpace

  override def toString():String = s"Constant($c)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.constant) ++ c.space.shape.serialize ++ c.serialize

  def apply(x:Vector[Nothing,S]): Vector[I,S] = c

  def feedback:VectorFunction[Nothing,+[Nothing,I],S] = DeadInput(domain+target)

}

object Constant {

  def apply[I,S](c:Vector[I,S]):Constant[I,S] = new Constant(c)

}
