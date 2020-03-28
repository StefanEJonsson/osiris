package osiris.utilities.serialization

import osiris.shape._
import osiris.utilities.serialization.v2

object Shape {

  def deserialize(bytes:Iterator[Byte]):Shape[_] = {
    v2.Shape.deserialize(bytes)
  }

}
