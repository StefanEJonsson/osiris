package osiris.utilities.serialization

object Morphism {

  def deserialize(bytes:Iterator[Byte]):osiris.morphism.Morphism[_,_] = {
    v2.Morphism.deserialize(bytes)
  }

}