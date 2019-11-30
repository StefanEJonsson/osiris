// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.reindex

import osiris._
import osiris.morphism.Monomorphism
import osiris.vector.space.{MatrixSpace, PairSpace, VectorSpace}

class Extract[A,B,S](t:VectorSpace[A,S], d:VectorSpace[B,S], f:Monomorphism[A,B])
  extends ReIndex(t,d,f) {

  def feedback:Pad[B,A,S] = new Pad(d,t,f)

}

object Extract {

  def element[I,S](domain:VectorSpace[I,S], i:I) =
    new Extract(I-->domain.scalarSpace,domain,morphism.constant(domain.shape,i))

  def left[L,R,S](domain:PairSpace[L,R,S]) =
    new Extract[L,+[L,R],S](domain.left,domain,morphism.sum.left(domain.left.shape,domain.right.shape))

  def right[L,R,S](domain:PairSpace[L,R,S]) =
    new Extract[R,+[L,R],S](domain.right,domain,morphism.sum.right(domain.left.shape,domain.right.shape))

  def row[I,J,S](domain:MatrixSpace[I,J,S],row:I) =
    new Extract[J,*[I,J],S](domain.inner,domain,morphism.product.rightPairedWith(domain.outer.shape,domain.inner.shape,row))

  def col[I,J,S](domain:MatrixSpace[I,J,S],col:J) =
    new Extract[I,*[I,J],S](domain.outer,domain,morphism.product.leftPairedWith(domain.outer.shape,domain.inner.shape,col))


}
