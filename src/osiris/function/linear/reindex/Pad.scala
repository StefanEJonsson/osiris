// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.reindex

import osiris._
import osiris.function.linear.LinearFunction
import osiris.morphism.Monomorphism
import osiris.utilities.serialization.v2
import osiris.vector.space.{MatrixSpace, PairSpace, VectorSpace}
import osiris.vector.{Single, Vector}

class Pad[I,J,S](s:ScalarSpace[S],f:Monomorphism[J,I]) extends LinearFunction[I,J,S] {

  override def toString():String = s"Pad($f)"

  val domain = f.domain --> s
  val target = f.target --> s

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.pad) ++ f.serialize

  def apply(x:Vector[J,S]):Vector[I,S] = (x | new Single(scalarSpace.zero)).reIndex(target,f.monoInverse)

  def feedback:Extract[J,I,S] = new Extract(s,f)

}

object Pad {

  def elem[I,S](target:VectorSpace[I,S],i:I) =
    new Pad(target.scalarSpace,morphism.constant(target.shape,i))

  def left[L,R,S](target:PairSpace[L,R,S]) =
    new Pad(target.scalarSpace,morphism.sum.left[L,R](target.left.shape,target.right.shape))

  def right[L,R,S](target:PairSpace[L,R,S]) =
    new Pad(target.scalarSpace,morphism.sum.right[L,R](target.left.shape,target.right.shape))

  def row[I,J,S](target:MatrixSpace[I,J,S],i:I) =
    new Pad(target.scalarSpace,morphism.product.rightPairedWith[I,J](target.outer.shape,target.inner.shape,i))

  def col[I,J,S](target:MatrixSpace[I,J,S],j:J) =
    new Pad(target.scalarSpace,morphism.product.leftPairedWith[I,J](target.outer.shape,target.inner.shape,j))

}