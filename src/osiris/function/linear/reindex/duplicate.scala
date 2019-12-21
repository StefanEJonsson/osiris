// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.reindex

import osiris.function.linear._
import osiris.morphism.product
import osiris.vector.Vector
import osiris.vector.space.VectorSpace
import osiris.{I, morphism}

class Copy[J,S](val domain:VectorSpace[J,S]) extends LinearFunction[Either[J,J],J,S] {

  val target = domain + domain

  def apply(x:Vector[J,S]):Vector[Either[J,J],S] = x | x

  def feedback = new Addition(domain)

}

class Fill[I,S](val target:VectorSpace[I,S]) extends LinearFunction[I,Unit,S] {

  val domain = I --> target.scalarSpace

  def apply(x:Vector[Unit,S]):Vector[I,S] = x.reIndex(target,morphism.unit[I](target.shape))

  def feedback = new Sum(target)

}

class RowCopy[I,J,S](outer:VectorSpace[I,S],inner:VectorSpace[J,S]) extends LinearFunction[(I,J),J,S] {

  val domain = inner
  val target = outer*inner

  def apply(x:Vector[J,S]):Vector[(I,J),S] = x.reIndex(target,product.second[I,J](outer.shape,inner.shape))

  def feedback = new ColSum(outer,inner)

}

class ColCopy[I,J,S](outer:VectorSpace[I,S],inner:VectorSpace[J,S]) extends LinearFunction[(I,J),I,S] {

  val domain = outer
  val target = outer*inner

  def apply(x:Vector[I,S]):Vector[(I,J),S] = x.reIndex(target,product.first[I,J](outer.shape,inner.shape))

  def feedback = new RowSum(outer,inner)

}