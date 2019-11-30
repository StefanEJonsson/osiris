// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.vector

import osiris._
import morphism._
import osiris.vector.space._

trait Matrix[I,J,S] extends container.Table[I,J,S] with Vector[(I,J),S] {

  /* ---------------------------------------------------------------------------------------------------------------- */

  override val outer:VectorSpace[I,S]
  override val inner:VectorSpace[J,S]

  override lazy val space:MatrixSpace[I,J,S] = outer * inner

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def transpose:Matrix[J,I,S] =
    this.reIndex[(J,I)](product.commute[J,I](inner.shape,outer.shape)).asMatrix

  override def row(i:I):Vector[J,S] = this.reIndex[J](product.rightPairedWith[I,J](outer.shape,inner.shape,i) )

  override def col(j:J):Vector[I,S] = this.reIndex[I](product.leftPairedWith[I,J](outer.shape,inner.shape,j) )

  /* ---------------------------------------------------------------------------------------------------------------- */

  def rowMap[J2](newInner:VectorSpace[J2,S],f:Vector[J,S] => Vector[J2,S]):Matrix[I,J2,S] =
    (outer * newInner).rows(i => f(row(i)))

  def colMap[I2](newOuter:VectorSpace[I2,S],f:Vector[I,S] => Vector[I2,S]):Matrix[I2,J,S] =
    (newOuter * inner).cols(j => f(col(j)))


  def rowMap(f:Vector[J,S] => S):Vector[I,S] = outer((i:I) => f(row(i)) )

  def colMap(f:Vector[I,S] => S):Vector[J,S] = inner((j:J) => f(col(j)) )

  /* ---------------------------------------------------------------------------------------------------------------- */

  def rowWise[JThat,JRes](target:VectorSpace[JRes,S], //TODO maybe add some dimension checking
                          op:(Vector[J,S],Vector[JThat,S]) => Vector[JRes,S]
                         )(that:Matrix[I,JThat,S]):Matrix[I,JRes,S] =
    (outer*target).rows(i => op(this.row(i),that.row(i)))

  def colWise[IThat,IRes](target:VectorSpace[IRes,S],
                          op:(Vector[I,S],Vector[IThat,S]) => Vector[IRes,S]
                         )(that:Matrix[IThat,J,S]):Matrix[IRes,J,S] =
    (target*inner).cols(j => op(this.col(j),that.col(j)))

  def rowWise[JThat](op:(Vector[J,S],Vector[JThat,S]) => S)(that:Matrix[I,JThat,S]):Vector[I,S] =
    outer((i:I) => op(this.row(i),that.row(i)))

  def colWise[IThat](op:(Vector[I,S],Vector[IThat,S]) => S)(that:Matrix[IThat,J,S]):Vector[J,S] =
    inner((j:J) => op(this.col(j),that.col(j)))

  /* ---------------------------------------------------------------------------------------------------------------- */

  def rowSum:Vector[I,S] = rowMap(_.sum)

  def colSum:Vector[J,S] = colMap(_.sum)

  /* ---------------------------------------------------------------------------------------------------------------- */

  def replaceRow(i:I,value:Vector[J,S]):Matrix[I,J,S] = new ReplacedRow(this,Map(i -> value))

  def replaceCol(j:J,value:Vector[I,S]):Matrix[I,J,S] = new ReplacedCol(this,Map(j -> value))

  /* ---------------------------------------------------------------------------------------------------------------- */

  def *(that:Vector[J,S]):Vector[I,S] = outer( (i:I) => this.row(i) <> that )

  def *[J0](that:Matrix[J,J0,S]):Matrix[I,J0,S] =
    (this.outer*that.inner)( (t:(I,J0)) =>
      this.row(t._1)<>that.col(t._2)
    )

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def asMatrix[I2,J2,P<:(I2,J2) with (I, J)]: Matrix[I2, J2, S] = this.asInstanceOf[Matrix[I2,J2,S]]

}

class UnCurry[I,J,S](original:container.Container[I,Vector[J,S]],val inner:VectorSpace[J,S])
  extends Matrix[I,J,S] {

  val outer = original.space.shape --> inner.scalarSpace

  def apply(t:((I,J))):S =  original(t._1)(t._2)

}

class ReplacedRow[I,J,S](original:Matrix[I,J,S],replacements:Map[I,Vector[J,S]]) extends Matrix[I,J,S] {

  val outer = original.outer
  val inner = original.inner

  def apply(t:(I,J)):S = replacements.get(t._1).map(_.apply(t._2)).getOrElse(original(t))

  override def replace(t:(I,J),value:S):Matrix[I,J,S] = {
    if (replacements.contains(t._1)) {
      val r = replacements + (t._1 -> replacements(t._1).replace(t._2,value))
      new ReplacedRow(original,r)
    } else {
      new ReplacedRow(original.replace(t,value).asMatrix,replacements)
    }
  }

  override def replaceRow(i:I, value:Vector[J,S]): Matrix[I,J,S] = {
    val r = replacements + (i -> value)
    if (r.size == outer.shape.size) {space((t:(I,J)) => r(t._1)(t._2))} else {new ReplacedRow(original,r)}
  }

  override def replaceCol(j:J, value:Vector[I,S]):Matrix[I,J,S] =
    new ReplacedRow(original.replaceCol(j,value),replacements)

}

class ReplacedCol[I,J,S](original:Matrix[I,J,S],replacements:Map[J,Vector[I,S]]) extends Matrix[I,J,S] {

  val outer = original.outer
  val inner = original.inner

  def apply(t:(I,J)):S = replacements.get(t._2).map(_.apply(t._1)).getOrElse(original(t))

  override def replace(t:(I,J),value:S):Matrix[I,J,S] = {
    if (replacements.contains(t._2)) {
      val r = replacements + (t._2 -> replacements(t._2).replace(t._1,value))
      new ReplacedCol(original,r)
    } else {
      new ReplacedCol(original.replace(t,value).asMatrix,replacements)
    }
  }

  override def replaceRow(i:I, value:Vector[J,S]): Matrix[I,J,S] =
    new ReplacedCol(original.replaceRow(i,value),replacements)

  override def replaceCol(j:J, value:Vector[I,S]):Matrix[I,J,S] = {
    val r = replacements + (j -> value)
    if (r.size == outer.shape.size) {space((t:(I,J)) => r(t._2)(t._1))} else {new ReplacedCol(original,r)}
  }

}