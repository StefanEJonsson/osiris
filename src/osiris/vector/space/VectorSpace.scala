// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.vector.space

import osiris._
import osiris.morphism._
import osiris.pin.Pin
import osiris.pin.variable.{ConstantSpace, ParameterSpace, VariableSpace}
import osiris.vector.{Single, Vector}

trait VectorSpace[I,S] extends container.companion.ContainerCompanion[I,S] {

  val scalarSpace:ScalarSpace[S]

  override def toString: String = s"$shape --> $scalarSpace"

  override def equals(that:Any):Boolean = that match {
    case (that:VectorSpace[I,S]) => that.scalarSpace == this.scalarSpace && that.shape == this.shape
    case _ => false
  }

  /* ---------------------------------------------------------------------------------------------------------------- */

  def open(file:String):Vector[I,S] = {
    import java.io.File
    import java.nio.file.Files

    val f = new File(file)
    val bytes = Files.readAllBytes(f.toPath)

    deserialize(bytes.iterator)
  }

  def apply(f:I=>S):Vector[I,S]

  def parameter(name:String):ParameterSpace[I,S] = new ParameterSpace(name,this)

  def variable:VariableSpace[I,S] = new VariableSpace(this)

  def constant:ConstantSpace[I,S] = new ConstantSpace(this)

  def apply(f:I=>Pin[Unit,S]):Pin[I,S] = {
    val merge = new osiris.pin.node.merge.Merge(this)
    for (i <- shape) {
      f(i) ->- merge.in(i)
    }
    merge.out
  }

  private[space] def parseElems(bytes: Iterator[Byte]):Vector[I,S]

  def deserialize(bytes:Iterator[Byte]):Vector[I,S] = {
    import utilities.Serialization._
    val v = bytes.next()
    if(v <= 0) {
      throw new IllegalArgumentException(s"illegal version number: $v. Version number has to be greater than 0")
    }
    if(v <= version) {
      val repr = bytes.next()
      if(repr == Representation.elems) {
        val res = parseElems(bytes)
        if (bytes.hasNext) {
          println(bytes.toVector.map(_.toChar).foldLeft("")((x:String,y:Char) => x + y))
          throw new Exception("trailing garbage")
        }
        res
      } else if (repr == Representation.reindex) {
        val s = osiris.shape.Shape.deserialize(bytes)
        val f = osiris.morphism.Morphism.deserialize(bytes)
        val v = (s-->scalarSpace).deserialize(bytes)
        new osiris.vector.ReIndexed[I,s.Type,S](v.asInstanceOf[Vector[s.Type,S]],f.asInstanceOf[Morphism[I,s.Type]]) //TODO test this!!
      } else {
        throw new IllegalArgumentException(s"illegal representation number $repr")
      }
    } else {
      throw new IllegalArgumentException(s"cannot deserialize byte string of version $v (this is version $version)")
    }
  }

  /* ---------------------------------------------------------------------------------------------------------------- */

  def fill(s:S):Vector[I,S] = new Single(s).reIndex(morphism.unit[I](shape))

  def zeros = fill(scalarSpace.zero)

  def ones = fill(scalarSpace.one)

  def unit(i:I):Vector[I,S] =
    (new Single(scalarSpace.zero) | new Single(scalarSpace.one)).reIndex(morphism.bool.equals(shape,i))

  def units(predicate:Morphism[I,bool.BOOL]) =
    (new Single(scalarSpace.zero) | new Single(scalarSpace.one)).reIndex(predicate)

  /* ---------------------------------------------------------------------------------------------------------------- */

  def +[J](that:VectorSpace[J,S]):PairSpace[I,J,S] = new PairSpace(this,that)

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