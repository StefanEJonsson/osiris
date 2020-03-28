package osiris.utilities.serialization

import osiris.function.{Constant, DeadInput, Identity, VectorFunction}
import osiris.function.bilinear.BilinearFunction
import osiris.function.linear.LinearFunction
import osiris.morphism.Isomorphism
import osiris._
import osiris.utilities.serialization.v2

object Function {

  def deserialize[I,J,S](bytes:Iterator[Byte]):VectorFunction[I,J,S] = {
    import Serialization._
    val v = bytes.next()
    checkVersion(v)
    v match {
      case 1 => throw new Exception("Trying to deserialize bytes as function. They claim to be version 1 but functions were not serializable in version 1")
      case 2 => v2.Function.deserialize(bytes)
    }
  }

}