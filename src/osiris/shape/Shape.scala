// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.shape

import osiris._
import osiris.container.companion.ContainerCompanion
import osiris.morphism.{Isomorphism, Monomorphism, Morphism}
import osiris.vector.space._

trait Shape[I] extends Iterable[I] {

  type Type = I

  def serialize:Iterable[Byte]

  def deserializeIndex(bytes:Iterator[Byte]):I

  def iterator:Iterator[I]

  def +[J](that:Shape[J]):Sum[I,J] = new Sum(this,that)
  def *[J](that:Shape[J]):Product[I,J] = new Product(this,that)

  def -->[S]():ContainerCompanion[I,S]
  def table[J,S](c:container.companion.ContainerCompanion[J,S]):container.companion.TableCompanion[I,J,S] =
    new container.companion.TableCompanion(this --> [S](),c)

  def -->[S](s:ScalarSpace[S]):VectorSpace[I,S]
  def -->[J,S](s:VectorSpace[J,S]):MatrixSpace[I,J,S] =
    new MatrixSpace(this --> s.scalarSpace,s)

}


object Shape {

  def serializeIndex[I](i:I):Iterable[Byte] = i match {
    case (_:Unit) => Iterable.empty
    case (i:Int)  => utilities.Serialization.Primitives.serializeInt(i)
    case (a,b)    => serializeIndex(a) ++ serializeIndex(b)
    case Left(l)  => Iterable(utilities.Serialization.Index.left ) ++ serializeIndex(l)
    case Right(r) => Iterable(utilities.Serialization.Index.right) ++ serializeIndex(r)
    case (xs:Set[_]) => utilities.Serialization.Primitives.serializeInt(xs.size) ++
      xs.map(x => serializeIndex(x)).fold(Iterable.empty)(_++_)
    case (map:Morphism[_,_]) => map.serialize
  }

  def deserialize(bytes:Iterator[Byte]):Shape[_] = {
    import utilities.Serialization.{Shape => S}
    bytes.next() match {
      case S.sum => {
        val a = deserialize(bytes)
        val b = deserialize(bytes)
        new Sum[a.Type,b.Type](a.asInstanceOf[Shape[a.Type]],b.asInstanceOf[Shape[b.Type]]) //TODO seems unnecessarily complicated
      }
      case S.product => {
        val a = deserialize(bytes)
        val b = deserialize(bytes)
        new Product(a,b)
      }
      case S.range => {
        val lower = utilities.Serialization.Primitives.deserializeInt(bytes)
        val upper = utilities.Serialization.Primitives.deserializeInt(bytes)
        new Range(lower,upper)
      }
      case S.empty => Empty
      case S.single => Single
    }
  }

}