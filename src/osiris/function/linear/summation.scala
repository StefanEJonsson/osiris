// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear

import osiris.function.linear.reindex.{ColCopy, Copy, Fill, RowCopy}
import osiris.utilities.serialization
import osiris.utilities.serialization.v2
import osiris.utilities.serialization.Serialization
import osiris.vector.Vector
import osiris.vector.space.VectorSpace
import osiris.{I, utilities}

/**
  * Takes a pair of vectors and produces a third vector as their sum.
  *
  * z(i) = x(i) + y(i)
  */
class Addition[I,S](val target:VectorSpace[I,S]) extends LinearFunction[I,Either[I,I],S] {

  val domain = target + target

  override def toString():String = s"+[$target]"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.addition) ++
    target.shape.serialize

  def apply(x:Vector[Either[I,I],S]):Vector[I,S] = {
    val xp = x.asPair[I,I,Either[I,I]]
    xp.left + xp.right
  }

  def linearFeedback = new Copy(target)

}

/**
  * Takes a vector and produces a singleton which is the sum of all elements of the vector.
  */
class Sum[J,S](val domain:VectorSpace[J,S]) extends LinearFunction[Unit,J,S] {

  val target = I --> domain.scalarSpace

  override def toString():String = s"sum[$domain]"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.sum) ++
    domain.shape.serialize

  def apply(x:Vector[J,S]):Vector[Unit,S] = target.fill(x.sum)

  def linearFeedback = new Fill(domain)

}

/**
  * Takes a matrix and produces the vector that is the sum of all rows of the matrix.
  */
class RowSum[I,J,S](outer:VectorSpace[I,S],inner:VectorSpace[J,S]) extends LinearFunction[I,(I,J),S] {

  val target = outer
  val domain = outer*inner

  override def toString():String = s"RowSum[$outer $inner]"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.rowSum) ++
    outer.shape.serialize ++ inner.shape.serialize

  def apply(x:Vector[(I,J),S]):Vector[I,S] = x.asMatrix.rowSum

  def linearFeedback = new ColCopy(outer,inner)

}

/**
  * Takes a matrix and produces the vector that is the sum of all columns of the matrix.
  */
class ColSum[I,J,S](outer:VectorSpace[I,S],inner:VectorSpace[J,S]) extends LinearFunction[J,(I,J),S] {

  val target = inner
  val domain = outer*inner

  override def toString():String = s"ColSum[$outer $inner]"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.colSum) ++
    outer.shape.serialize ++ inner.shape.serialize

  def apply(x:Vector[(I,J),S]):Vector[J,S] = x.asMatrix.colSum

  def linearFeedback = new RowCopy(outer,inner)

}