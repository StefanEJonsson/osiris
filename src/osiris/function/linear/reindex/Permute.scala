// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.reindex

import osiris.morphism.{Isomorphism, product}
import osiris.utilities.serialization.v2
import osiris.vector.space.VectorSpace
import osiris.{ScalarSpace, morphism}

class Permute[I,J,S](s:ScalarSpace[S],f:Isomorphism[I,J])
  extends ReIndex[I,J,S](s,f) {

  override def toString():String = s"Permute($f)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.permute) ++ f.serialize

  def linearFeedback = new Permute(s,f.inverse)

}

object Permute {

  def id[I, S](domain: VectorSpace[I, S]): Permute[I, I, S] =
    new Permute(domain.scalarSpace, morphism.id[I](domain.shape))

  def swap[L, R, S](left: VectorSpace[L, S],
                                 right: VectorSpace[R, S]): Permute[Either[R, L], Either[L, R], S] =
    new Permute(left.scalarSpace, morphism.sum.commute(right.shape,left.shape))

  def transpose[I,J,S](outer: VectorSpace[I, S],
                       inner: VectorSpace[J, S]): Permute[(J, I),(I, J),S] =
    new Permute(inner.scalarSpace,product.commute(inner.shape,outer.shape))

}

