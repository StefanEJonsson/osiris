// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin

import osiris.evaluator.Environment
import osiris.function.{ScalarFunction, VectorFunction}
import osiris.function.bilinear.{MatrixMatrixProduct, MatrixVectorProduct}
import osiris.function.linear.{ColSum, RowSum}
import osiris.function.linear.reindex.Permute
import osiris.function.map.{ColMap, ColWise, RowMap, RowWise}
import osiris.pin.node.Node
import osiris.pin.node.merge.RowMerge
import osiris.pin.node.replace.{ReplaceCol, ReplaceRow}
import osiris.pin.node.split.RowSplit
import osiris.pin.variable.Constant
import osiris.shape.Shape
import osiris.{Objective, O, function, utilities}
import osiris.vector.{Matrix, Vector}
import osiris.vector.space.{MatrixSpace, VectorSpace}

trait MatrixPin[I,J,S] extends Pin[(I,J),S] {

  val space:MatrixSpace[I,J,S]

  private var split = Option.empty[RowSplit[I,J,S]]

  /*----------------------------------------------------------------------------------------------------------------- */

  def transpose:MatrixPin[J,I,S] = Permute.transpose(space.outer,space.inner)(this).asMatrix

  def row(i:I):Pin[J,S] = {
    if(split.isEmpty) {
      split = Option(new RowSplit(space))
      this ->- split.get.in
    }
    split.get(i)
  }

  def col(j:J):Pin[I,S] = transpose.row(j)

  override def map(f:ScalarFunction[S]):MatrixPin[I,J,S] = super.map(f).asMatrix

  override def map(f:SinglePin[S]=>Pin[Unit,S]):MatrixPin[I,J,S] = super.map(f).asMatrix

  override def elemWise(op:VectorFunction[Unit,Either[Unit,Unit],S])(that:Pin[(I,J),S]):MatrixPin[I,J,S] =
    super.elemWise(op)(that).asMatrix

  override def elemWise(op:(SinglePin[S],SinglePin[S])=>Pin[Unit,S])(that:Pin[(I,J),S]):MatrixPin[I,J,S] =
    super.elemWise(op)(that).asMatrix

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def *(k:SinglePin[S]):MatrixPin[I,J,S] = super.*(k).asMatrix

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def +(that:Pin[(I,J),S]):MatrixPin[I,J,S] = super.+(that).asMatrix

  override def -(that:Pin[(I,J),S]):MatrixPin[I,J,S] = super.-(that).asMatrix

  override def unary_-():MatrixPin[I,J,S] = super.unary_-().asMatrix

  override def o(that:Pin[(I,J),S]):MatrixPin[I,J,S] = super.o(that).asMatrix

  /*----------------------------------------------------------------------------------------------------------------- */

  def rowMap[J2](f:VectorFunction[J2,J,S]):MatrixPin[I,J2,S] =
    new RowMap(space.outer,f)(this).asMatrix

  def rowMap[J2](f:Pin[J,S]=>Pin[J2,S]):MatrixPin[I,J2,S] =
    rowWise[Nothing,J2](
      (l:Pin[J,S],_:Pin[Nothing,S]) => f(l)
    )(
      (space.outer *[Nothing] (osiris.O --> space.scalarSpace)).constant.zeros
    )

  def colMap[I2](f:VectorFunction[I2,I,S]):MatrixPin[I2,J,S] =
    new ColMap(space.inner,f)(this).asMatrix

  def colMap[I2](f:Pin[I,S]=>Pin[I2,S]):MatrixPin[I2,J,S] = {
    this.transpose.rowMap(f).transpose
  }

  /* ---------------------------------------------------------------------------------------------------------------- */

  def rowWise[JThat,JRes](op:VectorFunction[JRes,Either[J,JThat],S])(that:MatrixPin[I,JThat,S]):MatrixPin[I,JRes,S] =
    new RowWise(space.outer,op)(this|that).asMatrix

  def colWise[IThat,IRes](op:VectorFunction[IRes,Either[I,IThat],S])(that:MatrixPin[IThat,J,S]):MatrixPin[IRes,J,S] =
    new ColWise(space.inner,op)(this|that).asMatrix

  def rowWise[JThat,JRes](f:(Pin[J,S],Pin[JThat,S]) => Pin[JRes,S])(that:Pin[(I,JThat),S]):MatrixPin[I,JRes,S] = {
    val outer = this
    val t = that.asMatrix[I,JThat,(I,JThat)]
    object left extends Pin[J, S] {
      val space = outer.space.inner
      lazy val node = new Node {
        val sockets = Set()
        lazy val pins = Set(left)
        def eval(environment: Environment): Unit = {}
        def rowWise[I](shape:Shape[I],matrixifiedPins:collection.mutable.Map[Pin[_,_],MatrixPin[I,_,_]]): Unit = {
          matrixifiedPins(left) =
            new function.linear.reindex.RowCopy(shape-->space.scalarSpace,space)(left).asMatrix
        }
      }
    }
    object right extends Pin[JThat, S] {
      val space = t.space.inner
      lazy val node = new Node {
        val sockets = Set()
        lazy val pins = Set(right)
        def eval(environment: Environment): Unit = {}
        def rowWise[I](shape:Shape[I],matrixifiedPins:collection.mutable.Map[Pin[_,_],MatrixPin[I,_,_]]): Unit = {
          matrixifiedPins(right) =
            new function.linear.reindex.RowCopy(shape-->space.scalarSpace,t.space.inner)(right).asMatrix[I,JThat,(I,JThat)]
        }
      }
    }

    val output = f(left,right)

    output.sum ->- new Objective(space.scalarSpace.zero)

    val internalNodes = collection.mutable.ListBuffer[Node]()
    val dependenceFront = collection.mutable.ListBuffer[Socket[_, _]]()

    val front = collection.mutable.Queue[Node]()
    front.enqueue(left.node); front.enqueue(right.node)

    while (front.nonEmpty) {
      val n = front.dequeue()
      if (!internalNodes.contains(n)) {
        internalNodes.append(n)
        if (n.pins.isEmpty) {
          dependenceFront.append(n.sockets.toSeq:_*)
        }
        val successors = n.pins.flatMap[Socket[_,_],Set[Socket[_,_]]](_.sockets).map(_.node)
        for (m <- successors) {
          front.enqueue(m)
        }
      }
    }

    val externalDependencies = collection.mutable.Set[Socket[_, _]]()
    val visited = collection.mutable.Set[Socket[_, _]]()
    val nodes = collection.mutable.ListBuffer[Node]()

    while (dependenceFront.nonEmpty) {
      val s = dependenceFront.head
      dependenceFront.remove(0)
      nodes -= s.node
      nodes.prepend(s.node)
      if (!visited.contains(s)) {
        visited += s

      }
      val pred = s.pin.get.node
      if (internalNodes.contains(pred)) {
        for (t <- pred.sockets) {
          dependenceFront -= t
          dependenceFront.append(t)
        }
      } else {
        externalDependencies += s
      }
    }

    val matrixifiedPins = collection.mutable.Map[Pin[_,_], MatrixPin[I,_,_]]()
    matrixifiedPins.put(left,this)
    matrixifiedPins.put(right,that.asMatrix)

    for (d <- externalDependencies) {
      type J = d.space.shape.Type
      type S = d.space.scalarSpace.Type
      val dep = d.asInstanceOf[Socket[J, S]]
      val pin: Pin[J, S] = dep.asInstanceOf[Socket[J, S]].pin.get
      val out = outer.space.outer.shape --> pin.space.scalarSpace
      val matrixPin = new osiris.function.linear.reindex.RowCopy[I,J,S](out, pin.space)(pin).asMatrix[I,J,(I,J)]
      matrixifiedPins.put(pin,matrixPin)
    }

    for (n <- nodes) {
      n.rowWise(outer.space.outer.shape, matrixifiedPins)
    }

    for (d <- externalDependencies) {
      type J = d.space.shape.Type
      type S = d.space.scalarSpace.Type
      val dep = d.asInstanceOf[Socket[J, S]]
      val pin: Pin[J, S] = dep.asInstanceOf[Socket[J, S]].pin.get
      pin -/- dep
    }

    matrixifiedPins(output).asInstanceOf[MatrixPin[I, JRes, S]]
  }

  def colWise[IThat,IRes](f:(Pin[I,S],Pin[IThat,S]) => Pin[IRes,S])(that:Pin[(IThat,J),S]):MatrixPin[IRes,J,S] =
    this.transpose.rowWise(f)(that.asMatrix.transpose).transpose

  /* ---------------------------------------------------------------------------------------------------------------- */

  def rowSum:Pin[I,S] = new RowSum(space.outer,space.inner)(this)

  def colSum:Pin[J,S] = new ColSum(space.outer,space.inner)(this)

  /* ---------------------------------------------------------------------------------------------------------------- */

  def replaceRow(i:I,value:Pin[J,S]):MatrixPin[I,J,S] = {
    val r = new ReplaceRow(space,i)
    this ->- r.in
    value ->- r.replacement
    r.out
  }

  def replaceCol(j:J,value:Pin[I,S]):MatrixPin[I,J,S] = {
    val r = new ReplaceCol(space,j)
    this ->- r.in
    value ->- r.replacement
    r.out
  }

  /* ---------------------------------------------------------------------------------------------------------------- */

  def *(that:Pin[J,S]):Pin[I,S] = MatrixVectorProduct(space.outer,space.inner)(this|that) //TODO dimension check

  def *[J0](that:MatrixPin[J,J0,S]):MatrixPin[I,J0,S] =
    MatrixMatrixProduct(
      this.space.outer,
      utilities.same(this.space.inner,that.space.outer),
      that.space.inner
    )(this|that).asMatrix

}
