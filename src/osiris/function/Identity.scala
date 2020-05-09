// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function

import osiris._
import osiris.utilities.serialization.v2
import vector._
import space.VectorSpace

/**
  * Returns the input without changing it.
  *
  * f(x) = x
  *
  */
class Identity[I,S](domain:VectorSpace[I,S])
  extends function.linear.reindex.Permute[I,I,S](domain.scalarSpace,morphism.id(domain.shape)) {

  override def toString():String = s"Id[$domain]"

  override def serialize:Iterable[Byte] = Iterable(v2.Function.constants.identity) ++ domain.shape.serialize

  override def apply(x:Vector[I,S]):Vector[I,S] = x

  override val linearFeedback = this

}
