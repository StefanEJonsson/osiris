// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function

import osiris._
import osiris.utilities.serialization.v2
import osiris.utilities.serialization.v2.Primitives

/**
  * Base class for functions that have scalar input and output.
  *
  * The functions have to be differentiable so that they can be used in the gradient descent algorithm.
  *
  * @tparam S scalar type.
  */
trait ScalarFunction[S] extends (S => S) {

  val space:ScalarSpace[S]

  def serialize:Iterable[Byte]

  def apply(x:S):S

  def <<(that:ScalarFunction[S]):ScalarFunction[S] = new Composition(this,that)

  def +(that:ScalarFunction[S]):ScalarFunction[S] = new Plus(this,that)

  def -(that:ScalarFunction[S]):ScalarFunction[S] = new Minus(this,that)

  def *(that:ScalarFunction[S]):ScalarFunction[S] = new Times(this,that)

  def /(that:ScalarFunction[S]):ScalarFunction[S] = new Div(this,that)

  def ^(that:ScalarFunction[S]):ScalarFunction[S] = new Power(this,that)

  def ^(k:Int):ScalarFunction[S] = Monomial(space,k) << this

  def unary_- :ScalarFunction[S] = new Neg(this)

  def inv:ScalarFunction[S] = new Inv(this)

  def derivative:ScalarFunction[S]

  /**
    * The composition of two scalar functions.
    *
    * @param outer the left argument of the composition operator (last function to be applied).
    * @param inner the right argument of the composition operator (first function to be applied).
    */
  case class Composition private[ScalarFunction] (outer:ScalarFunction[S],inner:ScalarFunction[S])
    extends ScalarFunction[S] {

    override def toString():String = s"($outer << $inner)"

    def serialize:Iterable[Byte] =
      Iterable(v2.ScalarFunction.composition) ++ outer.serialize ++ inner.serialize

    val space = utilities.same(outer.space,inner.space)

    def apply(x:S):S = outer(inner(x))

    def derivative:ScalarFunction[S] = (outer.derivative << inner) * inner.derivative

  }

  /**
    * The sum of two scalar functions.
    */
  case class Plus private[ScalarFunction]
  (f:ScalarFunction[S],g:ScalarFunction[S]) extends ScalarFunction[S] {

    override def toString():String = s"($f + $g)"

    def serialize:Iterable[Byte] =
      Iterable(v2.ScalarFunction.add) ++ f.serialize ++ g.serialize

    val space = utilities.same(f.space,g.space)

    def apply(x:S):S = space.+(f(x),g(x))

    def derivative: ScalarFunction[S] = f.derivative + g.derivative

  }

  /**
    * The difference of two scalar functions.
    */
  case class Minus private[ScalarFunction] (f:ScalarFunction[S],g:ScalarFunction[S])
    extends ScalarFunction[S] {

    override def toString():String = s"($f - $g)"

    def serialize:Iterable[Byte] =
      Iterable(v2.ScalarFunction.sub) ++ f.serialize ++ g.serialize

    val space = utilities.same(f.space,g.space)

    def apply(x:S):S = space.-(f(x),g(x))

    def derivative: ScalarFunction[S] = f.derivative - g.derivative

  }

  /**
    * The negation of a scalar function.
    *
    * Neg(f)(x) = -f(x)
    *
    */
  case class Neg private[ScalarFunction] (f:ScalarFunction[S]) extends ScalarFunction[S] {

    override def toString():String = s"-$f" //u- stands for unary minus. Maybe find better name?

    def serialize:Iterable[Byte] =
      Iterable(v2.ScalarFunction.neg) ++ f.serialize

    val space = f.space

    def apply(x:S):S = space.neg(f(x))

    def derivative: ScalarFunction[S] = -f.derivative

  }

  /**
    * The product of two scalar functions.
    */
  case class Times private[ScalarFunction] (f:ScalarFunction[S],g:ScalarFunction[S]) extends ScalarFunction[S] {

    override def toString():String = s"($f * $g)"

    def serialize:Iterable[Byte] =
      Iterable(v2.ScalarFunction.mul) ++ f.serialize ++ g.serialize

    val space = utilities.same(f.space,g.space)

    def apply(x:S):S = space.*(f(x),g(x))

    def derivative: ScalarFunction[S] = f.derivative * g + f * g.derivative

  }

  /**
    * The quotient of two scalar functions.
    */
  case class Div private[ScalarFunction] (f:ScalarFunction[S],g:ScalarFunction[S]) extends ScalarFunction[S] {

    override def toString():String = s"($f/$g)"

    def serialize:Iterable[Byte] =
      Iterable(v2.ScalarFunction.div) ++ f.serialize ++ g.serialize

    val space = utilities.same(f.space,g.space)

    def apply(x:S):S = {
      val a = f(x)
      val b = g(x)
      //if (a == space.zero && b == space.zero) { //TODO l'Hospital (doesn't work if infinite derivatives are zero)
      //  (f.derivative/g.derivative)(x)
      //} else {
        space./(a,b)
      //}
    }

    def derivative: ScalarFunction[S] = (f.derivative * g - f * g.derivative)/(g*g)

  }

  /**
    * The multiplicative inverse of a scalar function.
    *
    * Inv(f)(x) = 1/f(x)
    */
  case class Inv private[ScalarFunction] (f:ScalarFunction[S]) extends ScalarFunction[S] {

    override def toString():String = s"(1/$f)"

    def serialize:Iterable[Byte] =
      Iterable(v2.ScalarFunction.inv) ++ f.serialize

    val space = f.space

    def apply(x:S):S = space.inv(f(x))

    def derivative: ScalarFunction[S] = -f.derivative/(f*f)

  }

  /**
    * A function to the power of another function
    *
    * Power(f,g)(x) = f(x)`^`g(x)
    */
  case class Power private[ScalarFunction] (f: ScalarFunction[S], g: ScalarFunction[S])
    extends ScalarFunction[S] {

    override def toString():String = s"($f^$g)"

    def serialize:Iterable[Byte] =
      Iterable(v2.ScalarFunction.pow) ++ f.serialize ++ g.serialize

    val space = utilities.same(f.space,g.space)

    def apply(x:S):S = space.^(f(x),g(x))

    def derivative: ScalarFunction[S] = f.derivative * g * (f ^ (g - Const(space.one))) +
      g.derivative * (Ln(space) << f) * (f ^ g)

  }

}

object ScalarFunction {

  def serialize(f:ScalarFunction[_]):Iterable[Byte] =
    Iterable(utilities.serialization.Serialization.version) ++ v2.ScalarSpace.serialize(f.space) ++ f.serialize

}

/**
  * Takes scalar x and raises it to the kth power.
  */
case class Monomial[S] private (val space: ScalarSpace[S], k: Int) extends ScalarFunction[S] {

  override def toString():String = s"_^$k"

  override def equals(obj:Any):Boolean = obj match {
    case Monomial(s,k2) => space == s && k == k2
    case _ => false
  }

  def serialize:Iterable[Byte] =
    Iterable(v2.ScalarFunction.mon) ++ Primitives.serializeInt(k)

  def apply(x:S):S = space.^(x,k)

  def derivative: ScalarFunction[S] = Const(space.fromInt(k)) * Monomial(space,k-1)

}

object Monomial {

  def apply[S](space:ScalarSpace[S],k:Int):ScalarFunction[S] =
    if (k == 0) {Const(space.one)}
    else if (k == 1) {Id(space)}
    else {new Monomial[S](space,k)}

}

/**
  * The constant function that always returns c.
  */
case class Const[S] private (val c:S) extends ScalarFunction[S] {

  val space = ScalarSpace(c)

  def serialize: Iterable[Byte] = Iterable(v2.ScalarFunction.const) ++ space.serialize(c)

  override def toString():String = s"const $c"

  override def equals(obj:Any):Boolean = obj match {
    case Const(c2) => c == c2
    case _ => false
  }

  def apply(x:S):S = c

  def derivative: ScalarFunction[S] = new Const(space.zero)

}

object Const {

  def apply[S](c:S) = new Const(c)

}

/**
  * The identity function.
  *
  * f(x) = x
  *
  */
case class Id[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "id"

  override def equals(obj:Any):Boolean = obj match {
    case Id(s) => space == s
    case _ => false
  }

  def serialize: Iterable[Byte] = Iterable(v2.ScalarFunction.id)

  def apply(x:S):S = x

  def derivative: ScalarFunction[S] = Const(space.one)

}

object Id {

  def apply[S](sSpace:ScalarSpace[S]) = new Id(sSpace)

}

/**
  * The exponential function.
  */
case class Exp[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "exp"

  override def equals(obj:Any):Boolean = obj match {
    case Exp(s) => space == s
    case _ => false
  }

  def serialize: Iterable[Byte] = Iterable(v2.ScalarFunction.exp)

  def apply(x:S):S = space.exp(x)

  def derivative = Exp[S](space)

}

object Exp {

  def apply[S](space:ScalarSpace[S]) = new Exp(space)

}

/**
  * The natural logarithm function.
  *
  * Logarithm in base e, where e is Euler's number.
  */
case class Ln[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "ln"

  override def equals(obj:Any):Boolean = obj match {
    case Ln(s) => space == s
    case _ => false
  }

  def serialize: Iterable[Byte] = Iterable(v2.ScalarFunction.log)

  def apply(x:S):S = space.ln(x)

  def derivative = Id(space).inv

}

object Ln {

  def apply[S](space:ScalarSpace[S]) = new Ln(space)

}

/**
  * Sine function.
  */
case class Sin[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "sin"

  override def equals(obj:Any):Boolean = obj match {
    case Sin(s) => space == s
    case _ => false
  }

  def serialize: Iterable[Byte] = Iterable(v2.ScalarFunction.sin)

  def apply(x:S):S = space.sin(x)

  def derivative = Cos(space)

}

object Sin {

  def apply[S](space:ScalarSpace[S]) = new Sin(space)

}

/**
  * Cosine function.
  */
case class Cos[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "cos"

  override def equals(obj:Any):Boolean = obj match {
    case Cos(s) => space == s
    case _ => false
  }

  def serialize: Iterable[Byte] = Iterable(v2.ScalarFunction.cos)

  def apply(x:S):S = space.cos(x)

  def derivative = -Sin(space)

}

object Cos {

  def apply[S](space:ScalarSpace[S]) = new Cos(space)

  val R = F64

  val y = Sin(R)(4.0)

}

/**
  * Tangent function.
  */
case class Tan[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "tan"

  override def equals(obj:Any):Boolean = obj match {
    case Tan(s) => space == s
    case _ => false
  }

  def serialize: Iterable[Byte] = Iterable(v2.ScalarFunction.tan)

  def apply(x:S):S = space.tan(x)

  def derivative = (Cos(space)*Cos(space)).inv

}

object Tan {

  def apply[S](space:ScalarSpace[S]) = new Tan(space)

}


/**
  * Arcsine function - the inverse of the sine function.
  */
case class ArcSin[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "arcsin"

  override def equals(obj:Any):Boolean = obj match {
    case ArcSin(s) => space == s
    case _ => false
  }

  def serialize: Iterable[Byte] = Iterable(v2.ScalarFunction.asin)

  def apply(x:S):S = space.arcsin(x)

  def derivative = ((Const(space.one) - Id(space)*Id(space))^Const(space.fromDouble(0.5))).inv

}

object ArcSin {

  def apply[S](space:ScalarSpace[S]) = new ArcSin(space)

}

/**
  * Arccosine function - the inverse of the cosine function.
  */
case class ArcCos[S] private (space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "arccos"

  override def equals(obj:Any):Boolean = obj match {
    case ArcCos(s) => space == s
    case _ => false
  }

  def serialize: Iterable[Byte] = Iterable(v2.ScalarFunction.acos)

  def apply(x:S):S = space.arccos(x)

  def derivative = -ArcSin(space).derivative

}

object ArcCos {

  def apply[S](space:ScalarSpace[S]) = new ArcCos(space)

}

/**
  * arctangent function - the inverse of the tangent function.
  */
case class ArcTan[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "arctan"

  override def equals(obj:Any):Boolean = obj match {
    case ArcTan(s) => space == s
    case _ => false
  }

  def serialize: Iterable[Byte] = Iterable(v2.ScalarFunction.atan)

  def apply(x:S):S = space.arctan(x)

  def derivative = (Const(space.one) + Id(space)*Id(space)).inv

}

object ArcTan {

  def apply[S](space:ScalarSpace[S]) = new ArcTan(space)

}

/**
  * Absolute value.
  */
case class Abs[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "abs"

  override def equals(obj:Any):Boolean = obj match {
    case Abs(s) => space == s
    case _ => false
  }

  def serialize: Iterable[Byte] = Iterable(v2.ScalarFunction.abs)

  def apply(x:S):S = space.abs(x)

  def derivative = Const(space.fromDouble(2))* Heaviside(space) - Const(space.one)

}

object Abs {

  def apply[S](space:ScalarSpace[S]) = new Abs(space)

}

/**
  * Returns 1 for positive values of x and 0 for negative values.
  *
  * The Heaviside function is also called the unit step function.
  *
  * Note that its derivative is not computed in a distributional sense. It will not return diracs delta function.
  * The derivative is just constant zero.
  */
case class Heaviside[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "theta"

  override def equals(obj:Any):Boolean = obj match {
    case Heaviside(s) => space == s
    case _ => false
  }

  def serialize: Iterable[Byte] = Iterable(v2.ScalarFunction.heaviside)

  def apply(x:S):S = space.heaviside(x)

  def derivative = Const(space.zero)

}

object Heaviside {

  def apply[S](space:ScalarSpace[S]) = new Heaviside(space)

}