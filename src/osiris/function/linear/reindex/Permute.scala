// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.reindex

import osiris.morphism.{Isomorphism, constant, product}
import osiris.vector.space.{MatrixSpace, PairSpace, VectorSpace}
import osiris.{I, morphism}

class Permute[I,J,S](t:VectorSpace[I,S], d:VectorSpace[J,S], f:Isomorphism[I,J])
  extends ReIndex[I,J,S](t,d,f) {

  def feedback = new Permute(d,t,f.inverse)

}

object Permute {

  def id[I, S](domain: VectorSpace[I, S]): Permute[I, I, S] =
    new Permute(domain, domain, morphism.id[I](domain.shape))

  def swap[L, R, S](left: VectorSpace[L, S],
                                 right: VectorSpace[R, S]): Permute[Either[R, L], Either[L, R], S] =
    new Permute(right + left, left + right, morphism.sum.commute(right.shape,left.shape))

  def transpose[I,J,S](outer: VectorSpace[I, S],
                                  inner: VectorSpace[J, S]): Permute[(J, I),(I, J),S] =
    new Permute(inner*outer, outer*inner, product.commute(inner.shape,outer.shape))

}

