// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear

import osiris.utilities.serialization.v2
import osiris.vector.Vector

/**
  * The composition of two linear functions.
  *
  * Note that this does the same as osiris.function.Composition, but since outer and inner are required to be linear
  * ComposedLinear can extend LinearFunction.
  */
class ComposedLinear[I,K,J,S](outer:LinearFunction[I,K,S],inner:LinearFunction[K,J,S])
  extends LinearFunction[I,J,S] {

  val domain = inner.domain
  val target = outer.target

  override def toString():String = s"($outer << $inner)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.composedLinear) ++
    outer.serialize ++ inner.serialize

  def apply(x:Vector[J,S]):Vector[I,S] = outer(inner(x))

  def feedback = inner.feedback << outer.feedback

}
