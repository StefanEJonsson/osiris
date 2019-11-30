// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear

import osiris.function.linear.reindex.{ColCopy, Fill, Copy, RowCopy}
import osiris.vector.Vector
import osiris.vector.space.VectorSpace
import osiris.I

class Addition[I,S](val target:VectorSpace[I,S]) extends LinearFunction[I,Either[I,I],S] {

  val domain = target + target

  def apply(x:Vector[Either[I,I],S]):Vector[I,S] = {
    val xp = x.asPair[I,I,Either[I,I]]
    xp.left + xp.right
  }

  def feedback = new Copy(target)

}

class Sum[J,S](val domain:VectorSpace[J,S]) extends LinearFunction[Unit,J,S] {

  val target = I --> domain.scalarSpace

  def apply(x:Vector[J,S]):Vector[Unit,S] = target.fill(x.sum)

  def feedback = new Fill(domain)

  override def toString():String = s"Sum $domain"

}

class RowSum[I,J,S](outer:VectorSpace[I,S],inner:VectorSpace[J,S]) extends LinearFunction[I,(I,J),S] {

  val target = outer
  val domain = outer*inner

  def apply(x:Vector[(I,J),S]):Vector[I,S] = x.asMatrix.rowSum

  def feedback = new ColCopy(outer,inner)

}

class ColSum[I,J,S](outer:VectorSpace[I,S],inner:VectorSpace[J,S]) extends LinearFunction[J,(I,J),S] {

  val target = inner
  val domain = outer*inner

  def apply(x:Vector[(I,J),S]):Vector[J,S] = x.asMatrix.colSum

  def feedback = new RowCopy(outer,inner)

}