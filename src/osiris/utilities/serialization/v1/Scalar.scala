package osiris.utilities.serialization.v1

import osiris.ScalarSpace

object Scalar {

  def deserialize[S](space:ScalarSpace[S],bytes:Iterator[Byte]):S = (space match {
    case osiris.F32 => Primitives.deserializeFloat(bytes)
    case osiris.F64 => Primitives.deserializeDouble(bytes)
  }).asInstanceOf[S]

}
