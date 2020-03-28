package osiris.utilities.serialization.v2

import osiris.morphism.Morphism
import osiris.vector.{SimpleSequential, Vector}
import osiris.vector.space._

object Vector {

  val elems:Byte = 1
  val reindex:Byte = 2

  val pair:Byte = 11
  val rows:Byte = 15
  val cols:Byte = 16

  def deserialize[I,S](space:VectorSpace[I,S],bytes:Iterator[Byte]):Vector[I,S] = {
    val repr = bytes.next()
    repr match {
      case Vector.elems => parseElems(space,bytes)
      case Vector.reindex => {
        val s = osiris.utilities.serialization.v2.Shape.deserialize(bytes)
        val f = osiris.utilities.serialization.v2.Morphism.deserialize(bytes)
        val v = (s-->space.scalarSpace).deserialize(bytes)
        new osiris.vector.ReIndexed[I,s.Type,S](v.asInstanceOf[Vector[s.Type,S]],f.asInstanceOf[Morphism[I,s.Type]])
      }
      case Vector.pair => {
        val shape = space.shape.asInstanceOf[osiris.shape.Sum[_,_]]
        type L = shape.a.Type
        type R = shape.b.Type
        val pSpace = space.asInstanceOf[VectorSpace[Either[L,R],S]].asPairSpace[L,R,Either[L,R]]
        val left = deserialize(pSpace.left,bytes)
        val right = deserialize(pSpace.right,bytes)
        (left | right).asInstanceOf[Vector[I,S]]
      }
      case Vector.rows => {
        val shape = space.shape.asInstanceOf[osiris.shape.Product[_,_]]
        type II = shape.a.Type
        type J = shape.b.Type
        val mSpace = space.asInstanceOf[VectorSpace[(II,J),S]].asMatrixSpace[II,J,(II,J)]
        mSpace.rows((_:II) => deserialize(mSpace.inner,bytes)).asInstanceOf[Vector[I,S]]
      }
      case Vector.cols => {
        val shape = space.shape.asInstanceOf[osiris.shape.Product[_,_]]
        type II = shape.a.Type
        type J = shape.b.Type
        val mSpace = space.asInstanceOf[VectorSpace[(II,J),S]].asMatrixSpace[II,J,(II,J)]
        mSpace.cols((_:J) => deserialize(mSpace.outer,bytes)).asInstanceOf[Vector[I,S]]
      }
    }
  }

  def parseElems[I,S](space:VectorSpace[I,S],bytes:Iterator[Byte]):Vector[I,S] = (space match {
    case (e:EmptySpace[S]) => e()
    case (s:SingleSpace[S]) => s.apply((_:Unit) => Scalar.deserialize(space.scalarSpace,bytes))
    case (s:SequentialSpace[S]) =>
      new SimpleSequential(s,s.shape.map(_ => Scalar.deserialize(space.scalarSpace,bytes)).toVector)
    case (p:PairSpace[_,_,S]) => p.left.deserialize(bytes) | p.right.deserialize(bytes)
    case (m:MatrixSpace[_,_,S]) => {
      type I = m.outer.shape.Type
      type J = m.inner.shape.Type
      val mm = m.asInstanceOf[MatrixSpace[I,J,S]]
      mm.rows((_:I) => parseElems(mm.inner,bytes))
    }
  }).asInstanceOf[Vector[I,S]]

}
