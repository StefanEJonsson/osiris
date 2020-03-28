package osiris.utilities.serialization.v1

import osiris.morphism.Morphism
import osiris.utilities
import osiris.utilities.serialization.Serialization
import osiris.vector.{SimpleSequential, Vector}
import osiris.vector.space._

object Vector {

  val elems:Byte = 1
  val reindex:Byte = 2

  def deserialize[I,S](space:VectorSpace[I,S],bytes:Iterator[Byte]):Vector[I,S] = {
    import osiris.utilities.serialization.v1
    val repr = bytes.next()
    if (repr == v1.Vector.elems) {
      println("elems")
      val res = parseElems(space,bytes)
      if (bytes.hasNext) {
        println(bytes.toVector.map(_.toChar).foldLeft("")((x:String,y:Char) => x + y))
        throw new Exception("trailing garbage")
      }
      res
    } else if (repr == v1.Vector.reindex) {
      val s = utilities.serialization.v1.Shape.deserialize(bytes)
      val f = utilities.serialization.v1.Morphism.deserialize(bytes)
      val v = (s-->space.scalarSpace).deserialize(bytes)
      new osiris.vector.ReIndexed[I,s.Type,S](v.asInstanceOf[Vector[s.Type,S]],f.asInstanceOf[Morphism[I,s.Type]]) //TODO test this!!
    } else {
      throw new IllegalArgumentException(s"illegal representation number $repr")
    }
  }

  def parseElems[I,S](space:VectorSpace[I,S],bytes:Iterator[Byte]):Vector[I,S] = (space match {
    case (e:EmptySpace[S]) => e()
    case (s:SingleSpace[S]) => s.apply((_:Unit) => Scalar.deserialize(space.scalarSpace,bytes))
    case (s:SequentialSpace[S]) =>
      new SimpleSequential(s,s.shape.map(_ => Scalar.deserialize(space.scalarSpace,bytes)).toVector)
    case (p:PairSpace[_,_,S]) => parseElems(p.left,bytes) | parseElems(p.right,bytes)
    case (m:MatrixSpace[_,_,S]) => {
      type I = m.outer.shape.Type
      type J = m.inner.shape.Type
      val mm = m.asInstanceOf[MatrixSpace[I,J,S]]
      mm.rows((_:I) => parseElems(mm.inner,bytes))
    }
  }).asInstanceOf[Vector[I,S]]

}
