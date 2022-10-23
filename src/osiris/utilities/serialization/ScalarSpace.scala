package osiris.utilities.serialization

import osiris.ScalarSpace

object ScalarSpace {

  def deserialize(s:Iterator[Byte]):ScalarSpace[_] = v2.ScalarSpace.deserialize(s)

}