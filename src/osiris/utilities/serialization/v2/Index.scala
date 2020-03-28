package osiris.utilities.serialization.v2

import osiris.utilities.serialization.v2

object Index {

  val left :Byte = 1
  val right:Byte = 2

  def serializeIndex[I](i:I):Iterable[Byte] = i match {
    case (_:Unit) => Iterable.empty
    case (i:Int)  => Primitives.serializeInt(i)
    case (a,b)    => serializeIndex(a) ++ serializeIndex(b)
    case Left(l)  => Iterable(v2.Index.left ) ++ serializeIndex(l)
    case Right(r) => Iterable(v2.Index.right) ++ serializeIndex(r)
    case (xs:Set[_]) => Primitives.serializeInt(xs.size) ++
      xs.map(x => serializeIndex(x)).fold(Iterable.empty)(_++_)
    case (map:osiris.morphism.Morphism[_,_]) => map.serialize
  }

}