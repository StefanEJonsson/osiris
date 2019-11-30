// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.vector

import osiris.morphism
import osiris.morphism.Morphism

class AsEmpty[S](original:Vector[Nothing,S]) extends Empty[S](original.space.scalarSpace)

class AsSingle[S](original:Vector[Unit,S]) extends Single[S](original(()))

class AsSequential[S](original:Vector[Int,S]) extends Sequential[S] {

  val space = original.space.asSequentialSpace

  def apply(i:Int) = original(i)

  override def reIndex[I0](converter:Morphism[I0, Int]):Vector[I0, S] = original.reIndex(converter)

  override def replace(i: Int, value: S): Sequential[S] = original.replace(i,value).asSequential

}

class AsPair[L,R,J,S](original:Vector[Either[L,R],S])
  extends Pair[L,R,S] {

  val space = original.space.asPairSpace[L,R,Either[L,R]]

  lazy val left  = this.reIndex[L](space.left,morphism.sum.left[L,R](space.left.shape,space.right.shape))
  lazy val right = this.reIndex[R](space.right,morphism.sum.right[L,R](space.left.shape,space.right.shape))

  def apply(i:Either[L,R]) = original(i)

  override def reIndex[I0](converter: Morphism[I0, Either[L, R]]): Vector[I0, S] = original.reIndex(converter)

  override def replace(i: Either[L, R], value: S): Pair[L,R,S] = original.replace(i,value).asPair

}

class AsMatrix[R,C,S](original:Vector[(R,C),S]) extends Matrix[R,C,S] {

  val outer = original.space.asMatrixSpace.outer
  val inner = original.space.asMatrixSpace.inner

  def apply(i:(R,C)) = original(i)

  override def reIndex[I0](converter: Morphism[I0, (R, C)]): Vector[I0, S] = original.reIndex(converter)

  override def replace(i: (R,C),value:S):Matrix[R,C,S] = original.replace(i,value).asMatrix
  
}