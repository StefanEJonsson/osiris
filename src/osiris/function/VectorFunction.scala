// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function

import osiris._
import osiris.function.bilinear._
import osiris.function.linear.{Addition, ScalarProduct, Sum}
import osiris.function.linear.reindex.Extract
import vector._
import morphism._
import osiris.function.map.{BiMap, ElemWise}
import osiris.vector.space.{SingleSpace, VectorSpace}

trait VectorFunction[I,J,S] extends (Vector[J,S] => Vector[I,S]) {

  /* ---------------------------------------------------------------------------------------------------------------- */

  val domain:VectorSpace[J,S]
  val target:VectorSpace[I,S]

  protected def scalarSpace = utilities.same(domain.scalarSpace,target.scalarSpace)

  def apply(x:pin.Pin[J,S]):pin.Pin[I,S] = {
    val comp = new pin.node.FunctionNode(this)
    x ->- comp.in
    comp.out
  }

  /* ---------------------------------------------------------------------------------------------------------------- */

  def <<[J0](that:VectorFunction[J,J0,S]):VectorFunction[I,J0,S] = Composition(this,that)

  def |[I2,J2](that:VectorFunction[I2,J2,S]):VectorFunction[Either[I,I2],Either[J,J2],S] = new BiMap(this,that)

  /* ---------------------------------------------------------------------------------------------------------------- */

  def feedback(x:Vector[J,S],y:Vector[I,S]):Vector[J,S]

  /* ---------------------------------------------------------------------------------------------------------------- */

  import linear.reindex._

  def apply(i:I):VectorFunction[Unit,J,S] = Extract.element(target,i) << this

  def getLeft[L,R,II<:Either[L,R] with I] : VectorFunction[L,J,S] =
    Extract.left(target.asPairSpace[L,R,II]) << this.asInstanceOf[VectorFunction[Either[L,R],J,S]]

  def getRight[L,R,II<:Either[L,R] with I] : VectorFunction[R,J,S] =
    Extract.right(target.asPairSpace[L,R,II]) << this.asInstanceOf[VectorFunction[Either[L,R],J,S]]

  def row[Outer,Inner,II<:(Outer,Inner) with I](i:Outer):VectorFunction[Inner,J,S] =
    Extract.row[Outer,Inner,S](target.asMatrixSpace,i) <<
      this.asInstanceOf[VectorFunction[(Outer,Inner),J,S]]

  def col[Outer,Inner,II<:(Outer,Inner) with I](j:Inner):VectorFunction[Outer,J,S] =
    Extract.col[Outer,Inner,S](target.asMatrixSpace,j) <<
      this.asInstanceOf[VectorFunction[(Outer,Inner),J,S]]

  def map(f:ScalarFunction[S]):VectorFunction[I,J,S] = new function.map.Map(target,f) << this

  def elemWise[J2](op:VectorFunction[Unit,Either[Unit,Unit],S])(
    that:VectorFunction[I,J2,S]):VectorFunction[I,Either[J,J2],S] =
    new ElemWise(target,op) << (this | that)

  def +[J2](that:VectorFunction[I,J2,S]):VectorFunction[I,Either[J,J2],S] = new Addition(target) << (this|that)

  def -[J2](that:VectorFunction[I,J2,S]):VectorFunction[I,Either[J,J2],S] = this + -that

  def o[J2](that:VectorFunction[I,J2,S]):VectorFunction[I,Either[J,J2],S] =
    ElementWiseMultiplication(target) << (this | that)

  def unary_-():VectorFunction[I,J,S] = new ScalarProduct(target,scalarSpace.fromInt(-1)) << this

  /* ---------------------------------------------------------------------------------------------------------------- */

  def s:VectorFunction[Unit,J,S] = new Sum(target) << this

  def <>[J2](that:VectorFunction[I,J2,S]):VectorFunction[Unit,Either[J,J2],S] = (this o that).s

  def ><[I2,J2](that:VectorFunction[I2,J2,S]):VectorFunction[(I,I2),Either[J,J2],S] =
    OuterProduct(this.target,that.target) << (this | that)


  def -*||[I2,J2](that:VectorFunction[(I,I2),J2,S]):VectorFunction[I2,Either[J,J2],S] =
    VectorMatrixProduct(this.target,that.target.asMatrixSpace[I,I2,(I,I2)].inner) << (this | that) //TODO is there some better way (getting rid of type parameters)?

  def =*|[IL,IR,IP<:(IL,IR) with I,J2](that:VectorFunction[IR,J2,S]):VectorFunction[IL,Either[J,J2],S] =  //TODO some way of getting rid of 'asInstanceOf'??
    MatrixVectorProduct(this.target.asMatrixSpace[IL,IR,IP].outer,that.target) <<
      (this.asInstanceOf[VectorFunction[(IL,IR),J,S]] | that)

  def =*||[IL,IR,IP<:(IL,IR) with I,I2,J2](that:VectorFunction[(IR,I2),J2,S]):VectorFunction[(IL,I2),Either[J,J2],S] = { //TODO try to simplify
    val thisSpace = this.target.asMatrixSpace[IL, IR, IP]
    val thatSpace = that.target.asMatrixSpace[IR, I2, (IR, I2)]
    val middleSpace = utilities.same(thisSpace.inner, thatSpace.outer)
    val thisTyped = this.asInstanceOf[VectorFunction[(IL, IR), J, S]]
    MatrixMatrixProduct(thisSpace.outer, middleSpace, thatSpace.inner) << (thisTyped | that)
  }

}

case class Composition[I,K,J,S](outer:VectorFunction[I,K,S], inner:VectorFunction[K,J,S])
  extends VectorFunction[I,J,S] {

  val domain = inner.domain
  val middle = utilities.same(outer.domain, inner.target)
  val target = outer.target

  override def toString(): String = s"($outer < $inner)"

  def apply(x: Vector[J, S]): Vector[I, S] = outer(inner(x))

  def feedback(x:Vector[J,S],y:Vector[I,S]):Vector[J,S] = inner.feedback(x,outer.feedback(inner(x),y))

}