// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function

import osiris._
import osiris.utilities.serialization.v2
import vector.Vector
import vector.space.VectorSpace

/**
  * Takes any vector in domain as input and always returns the empty vector.
  *
  */
class DeadInput[J,S](val domain:VectorSpace[J,S]) extends VectorFunction[Nothing,J,S] {

  val target = O --> domain.scalarSpace

  override def toString():String = s"DeadInput[$domain]"

  def serialize:Iterable[Byte] = Iterable(v2.Function.constants.deadInput) ++ domain.shape.serialize

  def apply(x: Vector[J, S]): Vector[Nothing, S] = target((i:Nothing) => utilities.absurd(i))

  def feedback(x:Vector[J,S],y:Vector[Nothing,S]) = domain.zeros

}

object DeadInput {

  def apply[J,S](domain:VectorSpace[J,S]):DeadInput[J,S] = new DeadInput(domain)

}