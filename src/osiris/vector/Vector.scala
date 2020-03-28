// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.vector

import osiris._
import morphism._
import osiris.vector.space.VectorSpace
import container.Container
import osiris.utilities.serialization.Serialization

/**
  * A list of scalars, matrix of scalars or some other structure containing scalars. Every element in the structure has
  * an index. Elements are accessed using indices. Can be thought of as a finite mapping from the index set (the shape
  * of the vector) to the ScalarSpace.
  *
  * Every vector has a VectorSpace. The space of a vector specifies which type of scalars it can contain (ScalarSpace)
  * and which indices are used to access its elements.
  *
  * A typical 3-dimensional vector could for example belong to the space F32`^`range(1,3). F32 means that it contains
  * scalars of type Float and range(1,3) means that {1,2,3} is the index set (shape). If a vector x belongs to this space, then
  * x(1), x(2) and x(3) are Floats.
  *
  * In osiris, matrices are also considered to be vectors. The shape of a two by three matrix is {(1,1),(1,2),(1,3),
  * (2,1),(2,2),(2,3)}.
  *
  * @tparam I the type of indices used to access elements of the container.
  * @tparam S the type of elements stored in the container.
  */
trait Vector[I,S] extends Container[I,S] {

  /* ---------------------------------------------------------------------------------------------------------------- */

  val space:VectorSpace[I,S]

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def equals(t:Any):Boolean = t match {
    case (that: Vector[I, S]) => that.space == this.space && space.shape.forall(i => this (i) == that(i))
    case _ => false
  }

  def serialize:Iterable[Byte]

  def save(file:String): Unit = {
    import java.nio.file.{Files,Paths}
    utilities.createFile(file)
    Files.write(Paths.get(file),serialize.toArray)
  }

  /* ---------------------------------------------------------------------------------------------------------------- */

  /**
    * Concatenates two Vectors together to form a Pair.
    */
  def |[J](that:Vector[J,S]):Pair[I,J,S] = Pair(this,that)

  /* ---------------------------------------------------------------------------------------------------------------- */

  def map(f:S=>S):Vector[I,S] = space( (i:I) => f(this(i)) )

  def elemWise(op:(S,S) => S)(that:Vector[I,S]) = space( (i:I) => op(this(i),that(i)) )

  /**
    * Returns a matrix where the ith row is given by f(this(i)).
    *
    * @param inner the vector space of the rows of the resulting matrix
    * @param f the function that takes an element of this vector and produces the corresponding row for the resulting
    *          matrix.
    */
  def rows[J](inner:VectorSpace[J,S],f:S => Vector[J,S]):Matrix[I,J,S] = (space*inner).rows(i => f(this(i)))

  /**
    * Returns a matrix where the ith column is given by f(this(i))
    *
    * @param outer the vector space that of the columns of the resulting matrix.
    * @param f the function that takes an element of this vector and produces the corresponding column for the resulting
    *          matrix.
    */
  def cols[II](outer:VectorSpace[II,S],f:S => Vector[II,S]):Matrix[II,I,S] = (outer*space).cols(j => f(this(j)))

  private def fold(start:S)(op:(S,S) => S):S = space.shape.map(this(_)).fold(start)(op)

  /* ---------------------------------------------------------------------------------------------------------------- */

  def replace(i:I,value:S):Vector[I,S] = new Replaced(this,Map(i -> value))

  /* ---------------------------------------------------------------------------------------------------------------- */

  def *(k:S):Vector[I,S] = map((x:S) => space.scalarSpace.*(k,x))

  def *(k:Single[S]):Vector[I,S] = this*(k.value)

  /* ---------------------------------------------------------------------------------------------------------------- */

  def +(that:Vector[I,S]):Vector[I,S] = this.elemWise(space.scalarSpace.+)(that)

  def -(that:Vector[I,S]):Vector[I,S] = this.elemWise(space.scalarSpace.-)(that)

  def unary_-():Vector[I,S] = this.map((s:S) => space.scalarSpace.neg(s))

  /**
    * Element wise (index wise) multiplication.
    */
  def o(that:Vector[I,S]):Vector[I,S] = this.elemWise(space.scalarSpace.*)(that)

  /* ---------------------------------------------------------------------------------------------------------------- */

  def sum:S = this.fold(space.scalarSpace.zero)(space.scalarSpace.+)

  /**
    * Inner product.
    */
  def <>(that:Vector[I,S]) = (this o that).sum

  /**
    * Outer product.
    */
  def ><[J](that:Vector[J,S]):Matrix[I,J,S] =
    (this.space*that.space)( (t:(I,J)) => space.scalarSpace.*(this(t._1),that(t._2)) )

  def *[J](that:Matrix[I,J,S]):Vector[J,S] = that.inner( (j:J) => this <> that.col(j) )

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def reIndex[I0](converter: Morphism[I0,I]): Vector[I0,S] =
    new ReIndexed(this,converter)

  def reIndex[I0](target: VectorSpace[I0,S],converter: Morphism[I0,I]): Vector[I0,S] =
    new ReIndexed(this,converter)

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def asPair[L,R,E<:Either[L,R] with I]:Pair[L,R,S] = new AsPair(this.asInstanceOf[Vector[Either[L,R],S]])

  def asMatrix[I2,J,P<:(I2,J) with I]:Matrix[I2,J,S] = new AsMatrix(this.asInstanceOf[Vector[(I2,J),S]])

  override def asEmpty[n<:Nothing with I]:Empty[S] = new AsEmpty(this.asInstanceOf[Vector[Nothing,S]])

  override def asSingle[u<:Unit with I]:Single[S] = new AsSingle(this.asInstanceOf[Vector[Unit,S]])

  override def asSequential[int<:Int with I]:Sequential[S] = new AsSequential[S](this.asInstanceOf[Vector[Int,S]])

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def toRowVector:Matrix[Unit,I,S] =
    this.reIndex[(Unit,I)](product.getRight[I](space.shape)).asMatrix

  override def toColVector:Matrix[I,Unit,S] =
    this.reIndex[(I,Unit)](product.getLeft[I](space.shape)).asMatrix

}

object Vector {

  def serialize[I,S](vector:Vector[I,S]):Iterable[Byte] =
    Iterable(Serialization.version) ++ vector.serialize

}

class Replaced[I,S](original:Vector[I,S],replacements:Map[I,S]) extends Vector[I,S] {

  val space = original.space

  def apply(i:I):S = replacements.getOrElse(i,original(i))

  override def replace(i: I, value: S): Vector[I, S] = {
    val r = replacements + (i -> value)
    if (r.size == space.shape.size) {space((i:I) => r(i))} else {new Replaced(original,replacements)}
  }

  def serialize: Iterable[Byte] =
    Iterable(utilities.serialization.v2.Vector.elems) ++ iterator.flatMap(space.scalarSpace.serialize(_))

}