package osiris.utilities.serialization

import osiris.function._
import osiris.utilities.serialization

object ScalarFunction {

  def deserialize[S](f:Iterator[Byte]):ScalarFunction[S] = {
    val v = f.next()
    Serialization.checkVersion(v)
    v match {
      case 1 => throw new Exception("Trying to deserialize bytes as scalar function. They claim to be version 1 but functions were not serializable in version 1")
      case 2 => v2.ScalarFunction.deserialize(f)
    }
  }

}