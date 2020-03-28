package osiris.utilities.serialization.v2

import osiris.shape._
import osiris.utilities.serialization.v2

object Shape {

  val sum:Byte= 1
  val product:Byte = 2

  val empty:Byte = 3
  val single:Byte = 4
  val range:Byte = 5

  def deserialize(bytes:Iterator[Byte]):Shape[_] = {
    import v2.{Shape => S}
    bytes.next() match {
      case S.sum => {
        val a = deserialize(bytes)
        val b = deserialize(bytes)
        new Sum[a.Type,b.Type](a.asInstanceOf[Shape[a.Type]],b.asInstanceOf[Shape[b.Type]])
      }
      case S.product => {
        val a = deserialize(bytes)
        val b = deserialize(bytes)
        new Product(a,b)
      }
      case S.range => {
        val lower = Primitives.deserializeInt(bytes)
        val upper = Primitives.deserializeInt(bytes)
        new Range(lower,upper)
      }
      case S.empty => Empty
      case S.single => Single
    }
  }

}
