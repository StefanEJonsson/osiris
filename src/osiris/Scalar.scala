// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris

import osiris.utilities.serialization.v2.Primitives
import osiris.vector.space.SequentialSpace

import scala.reflect.ClassTag

trait ScalarSpace[S] {

  type Type = S

  val zero:S
  val one:S

  val tag:ClassTag[S]

  def ^(n:Int):SequentialSpace[S] = until(n) --> this

  def serialize(s:S):Iterable[Byte]

  def fromInt(i:Int):S
  def fromDouble(d:Double):S

  def toDouble(s:S):Double

  def isDefined(s:S):Boolean

  def +(a:S,b:S):S
  def -(a:S,b:S):S
  def *(a:S,b:S):S
  def /(a:S,b:S):S
  def ^(a:S,b:S):S
  def ^(a:S,b:Int):S

  def neg(a:S) :S
  def inv(a:S):S

  def exp(x:S):S
  def ln(x:S):S

  def sin(x:S):S
  def cos(x:S):S
  def tan(x:S):S

  def arcsin(x:S):S
  def arccos(x:S):S
  def arctan(x:S):S

  def abs(x:S):S

  def heaviside(x:S):S

}

object ScalarSpace {

  def apply[S](s:S):ScalarSpace[S] = s match {
    case (_:Float) => F32.asInstanceOf[ScalarSpace[S]]
    case (_:Double) => F64.asInstanceOf[ScalarSpace[S]]
    case _ => throw new Exception("unknown scalarSpace")
  }

  def apply[S]():ScalarSpace[S] =
    if (0f.isInstanceOf[S]) {F32.asInstanceOf[ScalarSpace[S]]}
    else if (0.0.isInstanceOf[S]) {F64.asInstanceOf[ScalarSpace[S]]}
    else {throw new Exception("No such scalarspace exists")}

}

object F64 extends ScalarSpace[Double] {

  val zero:Double = 0.0
  val one:Double = 1.0

  val tag = scala.reflect.classTag[Double]

  def fromDouble(d:Double):Double = d
  def fromInt(i:Int):Double = i
  def toDouble(d:Double):Double = d

  override def toString():String = "F64"
  def serialize(s:Double):Iterable[Byte] = Primitives.serializeDouble(s)

  def isDefined(s: Double): Boolean = !(s.isInfinite || s.isNaN)

  def +(a:Double,b:Double):Double = a + b
  def -(a:Double,b:Double):Double = a - b
  def *(a:Double,b:Double):Double = a * b
  def /(a:Double,b:Double):Double = a / b
  def ^(a:Double,b:Double):Double = math.pow(a,b)
  def ^(a:Double,b:Int):Double = Math.pow(a,b)

  def neg(a:Double):Double = -a
  def inv(a:Double):Double = 1.0/a

  def exp(s:Double):Double = math.exp(s)
  def ln(s:Double):Double = math.log(s)

  def sin(s:Double):Double = math.sin(s)
  def cos(s:Double):Double = math.cos(s)
  def tan(s:Double):Double = math.tan(s)

  def arcsin(s:Double):Double = math.asin(s)
  def arccos(s:Double):Double = math.acos(s)
  def arctan(s:Double):Double = math.atan(s)

  def abs(s:Double):Double = math.abs(s)

  def heaviside(s:Double):Double = if(s > 0.0) {1.0} else {0.0}

}

object F32 extends ScalarSpace[Float] {

  val zero:Float = 0.0f
  val one:Float = 1.0f

  val tag = scala.reflect.classTag[Float]

  def fromDouble(d:Double):Float = d.toFloat
  def fromInt(i:Int):Float = i
  def toDouble(f:Float):Double = f

  override def toString():String = "F32"
  def serialize(s:Float):Iterable[Byte] = Primitives.serializeFloat(s)

  def isDefined(s: Float): Boolean = !(s.isInfinite || s.isNaN)

  def +(a:Float,b:Float):Float = a + b
  def -(a:Float,b:Float):Float = a - b
  def *(a:Float,b:Float):Float = a * b
  def /(a:Float,b:Float):Float = a / b
  def ^(a:Float,b:Float):Float = Math.pow(a,b).toFloat
  def ^(a:Float,b:Int):Float = Math.pow(a,b).toFloat

  def neg(a:Float):Float = -a
  def inv(a:Float):Float = 1.0f/a

  def exp(s:Float):Float = math.exp(s).toFloat
  def ln(s:Float):Float = math.log(s).toFloat

  def sin(s:Float):Float = math.sin(s).toFloat
  def cos(s:Float):Float = math.cos(s).toFloat
  def tan(s:Float):Float = math.tan(s).toFloat

  def arcsin(s:Float):Float = math.asin(s).toFloat
  def arccos(s:Float):Float = math.acos(s).toFloat
  def arctan(s:Float):Float = math.atan(s).toFloat

  def abs(s:Float):Float = math.abs(s)

  def heaviside(s:Float):Float = if(s > 0.0) {1.0f} else {0.0f}

}