package osiris.utilities.serialization.v2

import osiris.ScalarSpace
import osiris.function._
import osiris.utilities.serialization.{Serialization, v2}

object ScalarFunction {

  val id:Byte = 1
  val const:Byte = 2

  val composition:Byte = 10

  val add:Byte = 21
  val mul:Byte = 22
  val sub:Byte = 23
  val div:Byte = 24
  val neg:Byte = 25
  val inv:Byte = 26
  val pow:Byte = 27
  val mon:Byte = 28

  val exp:Byte = 41
  val log:Byte = 42

  val sin:Byte = 51
  val cos:Byte = 52
  val tan:Byte = 53
  val asin:Byte = 54
  val acos:Byte = 55
  val atan:Byte = 56

  val abs:Byte = 71
  val heaviside:Byte = 72

  def deserialize[S](f:Iterator[Byte]):ScalarFunction[S] = {
    val space = v2.ScalarSpace.deserialize(f)
    if (space.isInstanceOf[ScalarSpace[S]]) {
      deserialize(space.asInstanceOf[ScalarSpace[S]],f)
    } else {
      throw new Exception(s"Trying to parse $space as ${osiris.ScalarSpace[S]()}")
    }
  }

  def deserialize[S](space: ScalarSpace[S], bytes: Iterator[Byte]):ScalarFunction[S] = {
    import v2.{ScalarFunction => S}
    bytes.next() match {
      case S.id => Id(space)
      case S.const => Const(Scalar.deserialize(space,bytes))
      case S.composition => {
        val f = deserialize(space,bytes)
        val g = deserialize(space,bytes)
        f << g
      }
      case S.add => {
        val f = deserialize(space,bytes)
        val g = deserialize(space,bytes)
        f + g
      }
      case S.mul => {
        val f = deserialize(space,bytes)
        val g = deserialize(space,bytes)
        f * g
      }
      case S.sub => {
        val f = deserialize(space,bytes)
        val g = deserialize(space,bytes)
        f - g
      }
      case S.div => {
        val f = deserialize(space,bytes)
        val g = deserialize(space,bytes)
        f / g
      }
      case S.pow => {
        val f = deserialize(space,bytes)
        val g = deserialize(space,bytes)
        f ^ g
      }
      case S.neg => {
        val f = deserialize(space,bytes)
        -f
      }
      case S.inv => {
        val f = deserialize(space,bytes)
        f.inv
      }
      case S.mon => {
        val k = Primitives.deserializeInt(bytes)
        Monomial(space,k)
      }
      case S.exp => Exp(space)
      case S.log => Ln(space)
      case S.sin => Sin(space)
      case S.cos => Cos(space)
      case S.tan => Tan(space)
      case S.asin => ArcSin(space)
      case S.acos => ArcCos(space)
      case S.atan => ArcTan(space)
      case S.abs => Abs(space)
      case S.heaviside => Heaviside(space)
    }
  }

}