// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.reindex

import osiris.function.linear._
import osiris.morphism.product
import osiris.utilities.serialization.v2
import osiris.vector.Vector
import osiris.vector.space.VectorSpace
import osiris.{I, morphism}

/**
  * Takes a vector and produces a Pair containing two copies of the original vector.
  * @param domain the vector space of the input vector.
  * @tparam J the index type for the input vector.
  * @tparam S the scalar type.
  */
class Copy[J,S](val domain:VectorSpace[J,S]) extends LinearFunction[Either[J,J],J,S] {

  val target = domain + domain

  override def toString():String = s"Copy[$domain]"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.copy) ++
    domain.shape.serialize

  def apply(x:Vector[J,S]):Vector[Either[J,J],S] = x | x

  def feedback = new Addition(domain)

}

/**
  * Takes a singleton x and produces a vector of a given shape containing only x.
  * @param target the vector space of the output.
  * @tparam I the index type for the output.
  * @tparam S the scalar type.
  */
class Fill[I,S](val target:VectorSpace[I,S]) extends LinearFunction[I,Unit,S] {

  val domain = I --> target.scalarSpace

  override def toString():String = s"Fill[$target]"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.fill) ++
      target.shape.serialize

  def apply(x:Vector[Unit,S]):Vector[I,S] = x.reIndex(target,morphism.unit[I](target.shape))

  def feedback = new Sum(target)

}

/**
  * Takes a vector x and produces a matrix where all the rows are x.
  * @param outer the vector space of the columns of the resulting matrix.
  * @param inner the vector space of the rows of the resulting matrix.
  * @tparam I the index type for the columns of the matrix.
  * @tparam J the index type for the rows of the matrix.
  * @tparam S the scalar type.
  */
class RowCopy[I,J,S](outer:VectorSpace[I,S],inner:VectorSpace[J,S]) extends LinearFunction[(I,J),J,S] {

  val domain = inner
  val target = outer*inner

  override def toString():String = s"RowCopy[$outer $inner]"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.rowCopy) ++
    outer.shape.serialize ++ inner.shape.serialize

  def apply(x:Vector[J,S]):Vector[(I,J),S] = x.reIndex(target,product.second[I,J](outer.shape,inner.shape))

  def feedback = new ColSum(outer,inner)

}

/**
  * Takes a vector x and produces a matrix where all the columns are x.
  * @param outer the vector space of the columns of the resulting matrix.
  * @param inner the vector space of the rows of the resulting matrix.
  * @tparam I the index type for the columns of the matrix.
  * @tparam J the index type for the rows of the matrix.
  * @tparam S the scalar type.
  */
class ColCopy[I,J,S](outer:VectorSpace[I,S],inner:VectorSpace[J,S]) extends LinearFunction[(I,J),I,S] {

  val domain = outer
  val target = outer*inner

  override def toString():String = s"ColCopy[$outer $inner]"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.colCopy) ++
    outer.shape.serialize ++ inner.shape.serialize

  def apply(x:Vector[I,S]):Vector[(I,J),S] = x.reIndex(target,product.first[I,J](outer.shape,inner.shape))

  def feedback = new RowSum(outer,inner)

}