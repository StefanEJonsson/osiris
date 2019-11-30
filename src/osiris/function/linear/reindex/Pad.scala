// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.reindex

import osiris._
import osiris.function.linear.LinearFunction
import osiris.morphism.Monomorphism
import osiris.vector.space.{MatrixSpace, PairSpace, VectorSpace}
import osiris.vector.{Single, Vector}

class Pad[I,J,S](val target:VectorSpace[I,S],val domain:VectorSpace[J,S],
                            f:Monomorphism[J,I]) extends LinearFunction[I,J,S] {

  def apply(x:Vector[J,S]):Vector[I,S] = (x | new Single(scalarSpace.zero)).reIndex(target,f.monoInverse)

  def feedback:Extract[J,I,S] = new Extract(domain,target,f)

}

object Pad {

  def elem[I,S](target:VectorSpace[I,S],i:I) =
    new Pad(target,I-->target.scalarSpace,morphism.constant(target.shape,i))

  def left[L,R,S](target:PairSpace[L,R,S]) =
    new Pad(target,target.left,morphism.sum.left[L,R](target.left.shape,target.right.shape))

  def right[L,R,S](target:PairSpace[L,R,S]) =
    new Pad(target,target.right,morphism.sum.right[L,R](target.left.shape,target.right.shape))

  def row[I,J,S](target:MatrixSpace[I,J,S],i:I) =
    new Pad(target,target.inner,morphism.product.rightPairedWith[I,J](target.outer.shape,target.inner.shape,i))

  def col[I,J,S](target:MatrixSpace[I,J,S],j:J) =
    new Pad(target,target.outer,morphism.product.leftPairedWith[I,J](target.outer.shape,target.inner.shape,j))

}