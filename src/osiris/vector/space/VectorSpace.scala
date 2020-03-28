// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.vector.space

import osiris._
import osiris.morphism._
import osiris.pin.Pin
import osiris.pin.variable.{ConstantSpace, ParameterSpace, VariableSpace}
import osiris.vector.{Single, Vector}

/**
  * VectorSpaces in osiris are mainly used as factory objects to construct vectors.
  */
trait VectorSpace[I,S] extends container.companion.ContainerCompanion[I,S] {

  val scalarSpace:ScalarSpace[S]

  override def toString: String = s"$shape --> $scalarSpace"

  override def equals(that:Any):Boolean = that match {
    case (that:VectorSpace[I,S]) => that.scalarSpace == this.scalarSpace && that.shape == this.shape
    case _ => false
  }

  /* ---------------------------------------------------------------------------------------------------------------- */

  /**
    * Opens a Vector from a file.
    *
    * @param file the full path of the file.
    */
  def open(file:String):Vector[I,S] = {
    import java.io.File
    import java.nio.file.Files

    val f = new File(file)
    val bytes = Files.readAllBytes(f.toPath)

    deserialize(bytes.iterator)
  }

  /**
    * Creates a new Vector containing the scalar f(i) at every index i.
    *
    * @param f a function or lambda expression specifying the elements of the vector at every given index.
    */
  def apply(f:I=>S):Vector[I,S]

  /**
    * Creates a [osiris.pin.variable.ParameterSpace] with the same shape and scalar space as this. The parameter space
    * is in turn used as a factory object to construct parameters belonging to this space.
    */
  def parameter(name:String):ParameterSpace[I,S] = new ParameterSpace(name,this)

  /**
    * Creates a [osiris.pin.variable.VariableSpace] with the same shape and scalar space as this. The VariableSpace
    * is in turn used as a factory object to construct variables belonging to this space.
    */
  def variable:VariableSpace[I,S] = new VariableSpace(this)

  /**
    * Creates a [osiris.pin.variable.ConstantSpace] with the same shape and scalar space as this. The ConstantSpace
    * is in turn used as a factory object to construct constants belonging to this space.
    */
  def constant:ConstantSpace[I,S] = new ConstantSpace(this)

  /**
    * Creates a new Pin containing f(i) at every index i.
    *
    * @param f a function or lambda expression specifying the elements of the vector at every given index.
    */
  def apply(f:I=>Pin[Unit,S]):Pin[I,S] = {
    val merge = new osiris.pin.node.merge.Merge(this)
    for (i <- shape) {
      f(i) ->- merge.in(i)
    }
    merge.out
  }

  def deserialize(bytes: Iterator[Byte]):Vector[I,S] = utilities.serialization.Vector.deserialize(this,bytes)

  /* ---------------------------------------------------------------------------------------------------------------- */

  /**
    * Creates a new Vector containing the element s at all indices.
    */
  def fill(s:S):Vector[I,S] = new Single(s).reIndex(morphism.unit[I](shape))

  /**
    * Returns the vector containing only zeros.
    */
  def zeros = fill(scalarSpace.zero)

  /**
    * Returns the vector containing only ones.
    */
  def ones = fill(scalarSpace.one)

  /**
    * Returns the vector containing a one at index i, and zeros everywhere else.
    */
  def unit(i:I):Vector[I,S] =
    (new Single(scalarSpace.zero) | new Single(scalarSpace.one)).reIndex(morphism.bool.equals(shape,i))

  /**
    * Returns the vector that contains ones for every index for which predicate returns true and a zero for every index
    * where predicate returns false.
    */
  def units(predicate:Morphism[I,bool.BOOL]) =
    (new Single(scalarSpace.zero) | new Single(scalarSpace.one)).reIndex(predicate)

  /* ---------------------------------------------------------------------------------------------------------------- */

  /**
    * The tensor sum of two vector spaces.
    *
    * The resulting space is used to construct [osiris.vector.Pair]s where the left component of every pair belongs to
    * this space and the right component belongs to that space.
    */
  def +[J](that:VectorSpace[J,S]):PairSpace[I,J,S] = new PairSpace(this,that)

  /**
    * The tensor product of two vector spaces.
    *
    * The resulting space is a matrix space.
    */
  def *[J](that:VectorSpace[J,S]):MatrixSpace[I,J,S] = new MatrixSpace(this,that)

  /* ---------------------------------------------------------------------------------------------------------------- */

  def asPairSpace[L,R,E<:Either[L,R] with I]:PairSpace[L,R,S] = this.asInstanceOf[PairSpace[L,R,S]]

  def asMatrixSpace[Outer,Inner,P<:(Outer,Inner) with I]:MatrixSpace[Outer,Inner,S] =
    this.asInstanceOf[MatrixSpace[Outer,Inner,S]]

  def asEmptySpace[n<:Nothing with I]:EmptySpace[S] = this.asInstanceOf[EmptySpace[S]]

  def asSingleSpace[u<:Unit with I]:SingleSpace[S] = this.asInstanceOf[SingleSpace[S]]

  def asSequentialSpace[int<:Int with I]:SequentialSpace[S] = this.asInstanceOf[SequentialSpace[S]]

  /* ---------------------------------------------------------------------------------------------------------------- */

}