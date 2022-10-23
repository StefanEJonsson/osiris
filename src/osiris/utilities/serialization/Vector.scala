package osiris.utilities.serialization

import osiris.vector.Vector
import osiris.vector.space._

object Vector {

  def deserialize[I,S](space:VectorSpace[I,S],bytes:Iterator[Byte]):Vector[I,S] = {
    val v = bytes.next()
    Serialization.checkVersion(v)
    v match {
      case 1 => {println(1); v1.Vector.deserialize(space,bytes)}
      case 2 => v2.Vector.deserialize(space,bytes)
    }
  }


}
