package osiris.utilities.serialization.v2

import osiris.{F32, F64, ScalarSpace}

object ScalarSpace {

  val f32:Byte = 5
  val f64:Byte = 6

  def serialize(s:ScalarSpace[_]):Iterable[Byte] = Iterable(s match {
    case F32 => f32
    case F64 => f64
  })

  def deserialize(s:Iterator[Byte]):ScalarSpace[_] = s.next() match {
    case this.f32 => F32
    case this.f64 => F64
  }

}