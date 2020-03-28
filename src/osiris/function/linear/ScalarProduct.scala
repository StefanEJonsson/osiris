// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear

import osiris.utilities.serialization.v2
import osiris.vector.Vector
import osiris.vector.space.VectorSpace

/**
  * Multiplies the input vector by a scalar k.
  *
  * This function differs from the two bilinear ScalarProducts because here k is considered to be a constant and not a
  * part of the input.
  */
class ScalarProduct[I,S](val domain:VectorSpace[I,S],k:S) extends LinearFunction[I,I,S] {

  val target = domain

  override def toString():String = s"$k*[$domain]"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.scalarProduct) ++
    domain.shape.serialize ++ domain.scalarSpace.serialize(k)

  def apply(x:Vector[I,S]):Vector[I,S] = x*k

  def feedback = this

}
