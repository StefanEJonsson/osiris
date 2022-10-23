// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin

import osiris._
import vector._
import function._
import osiris.evaluator.environment.VectorEnvironment
import osiris.morphism._
import osiris.pin.node.Node
import osiris.pin.node.merge.{Merge, PairMerge}
import osiris.pin.node.split.Split
import osiris.vector.space._

/**
  * Represents a value in a computation graph.
  *
  * A pin can be a Constant, Variable, Parameter or an output from a function.
  *
  * Computation graphs are built by connecting pins to [Socket]s. Sockets recieve data from pins during forward
  * propagation and produce feedback for pins during backpropagation.
  */
trait Pin[I,S] {

  val space:VectorSpace[I,S]

  val node:Node

  /* ---------------------------------------------------------------------------------------------------------------- */

  private val secondaryPins = collection.mutable.Set[Pin[_,S]]()
  private val s = collection.mutable.Set[Socket[I,S]]()

  def sockets = s.toSet

  def secondary = secondaryPins.toSet

  def ->-(socket:Socket[I,S]): Unit = if(this.space == socket.space) {
    s += socket
    socket.connect(this)
  } else {
    throw new Exception(s"Vector Space mismatch. Trying to connect pin in space $space to socket in space ${socket.space}")
  }

  def -/-(socket:Socket[I,S]):Unit = {
    s -= socket
    socket.disconnect()
  }

  def addSecondary(pin:Pin[_,S]*):Unit = {secondaryPins ++= pin}

  /* ---------------------------------------------------------------------------------------------------------------- */

  def apply(i:I):SinglePin[S] = function.linear.reindex.Extract.element(space,i)(this).asSingle

  def |[R](that:Pin[R,S]):PairPin[I,R,S] = {
    val merge = new PairMerge(this.space,that.space)
    this ->- merge.left
    that ->- merge.right
    merge.out
  }

  /* ---------------------------------------------------------------------------------------------------------------- */

  def map(f:ScalarFunction[S]):Pin[I,S] = new function.map.Map(space,f)(this)

  def map(f:SinglePin[S] => Pin[Unit,S]):Pin[I,S] = toColVector.rowMap(x => f(x.asSingle)).col()

  def elemWise(op:VectorFunction[Unit,Either[Unit,Unit],S])(that:Pin[I,S]) =
    new function.map.ElemWise(space,op)(this|that)

  def elemWise(op:(SinglePin[S],SinglePin[S]) => Pin[Unit,S])(that:Pin[I,S]):Pin[I,S] =
    this.toColVector.rowWise((x:Pin[Unit,S],y:Pin[Unit,S]) => op(x.asSingle,y.asSingle))(that.toColVector).col()

  /* ---------------------------------------------------------------------------------------------------------------- */

  def replace(i:I,value:Pin[Unit,S]):Pin[I,S] = {
    val r = new osiris.pin.node.replace.Replace(space,i)
    this ->- r.in
    value ->- r.replacement
    r.out
  }

  /* ---------------------------------------------------------------------------------------------------------------- */

  def *(k:SinglePin[S]):Pin[I,S] = bilinear.LeftScalarProduct(space)(k|this)

  def *(k:S):Pin[I,S] = new linear.ScalarProduct(space,k)(this)

  /* ---------------------------------------------------------------------------------------------------------------- */

  def +(that:Pin[I,S]):Pin[I,S] = new linear.Addition(space)(this|that)

  def -(that:Pin[I,S]):Pin[I,S] = this + -that

  def unary_-():Pin[I,S] = new linear.ScalarProduct(space,space.scalarSpace.fromDouble(-1))(this)

  def o(that:Pin[I,S]):Pin[I,S] = bilinear.ElementWiseMultiplication(space)(this|that)

  /* ---------------------------------------------------------------------------------------------------------------- */

  def sum:SinglePin[S] = new linear.Sum(space)(this).asSingle

  def <>(that:Pin[I,S]):SinglePin[S] = bilinear.InnerProduct(space)(this|that).asSingle

  def ><[J](that:Pin[J,S]):MatrixPin[I,J,S] = bilinear.OuterProduct(this.space,that.space)(this|that).asMatrix

  def *[J](that:MatrixPin[I,J,S]):Pin[J,S] =
    bilinear.VectorMatrixProduct(
      utilities.same(this.space,that.space.outer),
      that.space.inner
    )(this|that)

  /* ---------------------------------------------------------------------------------------------------------------- */

  def permute[I0](converter: Isomorphism[I0,I]): Pin[I0,S] =
    new linear.reindex.Permute(space.scalarSpace,converter)(this)

  /* ---------------------------------------------------------------------------------------------------------------- */

  def asPair[L,R,E<:Either[L,R] with I]:PairPin[L,R,S] = {
    val cast = new pin.node.cast.PairCast[L,R,S](space.asPairSpace)
    this.asInstanceOf[Pin[Either[L,R],S]] ->- cast.in
    cast.out
  }

  def asMatrix[I2,J,P<:(I2,J) with I]:MatrixPin[I2,J,S] = {
    val cast = new pin.node.cast.MatrixCast[I2,J,S](space.asMatrixSpace)
    this.asInstanceOf[Pin[(I2,J),S]] ->- cast.in
    cast.out
  }

  def asEmpty[n<:Nothing with I]:EmptyPin[S] = {
    val cast = new pin.node.cast.EmptyCast[S](space.asEmptySpace)
    this.asInstanceOf[Pin[Nothing,S]] ->- cast.in
    cast.out
  }

  def asSingle[u<:Unit with I]:SinglePin[S] = {
    val cast = new pin.node.cast.SingleCast[S](space.asSingleSpace)
    this.asInstanceOf[Pin[Unit,S]] ->- cast.in
    cast.out
  }

  def asSequential[int<:Int with I]:SequentialPin[S] = {
    val cast = new pin.node.cast.SequentialCast[S](space.asSequentialSpace)
    this.asInstanceOf[Pin[Int,S]] ->- cast.in
    cast.out
  }

  /* ---------------------------------------------------------------------------------------------------------------- */

  def toRowVector:MatrixPin[Unit,I,S] =
    this.permute[(Unit,I)](product.getRight[I](space.shape)).asMatrix

  def toColVector:MatrixPin[I,Unit,S] =
    this.permute[(I,Unit)](product.getLeft[I](space.shape)).asMatrix

}