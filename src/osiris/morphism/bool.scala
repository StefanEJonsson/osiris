// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.morphism

import osiris._
import osiris.shape.Shape
import osiris.utilities.serialization
import osiris.utilities.serialization.v2
import osiris.utilities.serialization.Serialization

/**
  * Important morphisms associated with Either[Unit,Unit], here used as an alternative implementation of Boolean.
  */
object bool {

  type BOOL = Either[Unit, Unit]

  val FALSE = Left(())
  val TRUE = Right(())

  implicit def boolToEither(x:Boolean):BOOL = if(x) {TRUE} else {FALSE}

  implicit def eitherToBool(x:BOOL):Boolean = x match {
    case  Left(()) => false
    case Right(()) => true
  }

  def equal[A](shape:Shape[A]) = new Morphism[(A,A),BOOL] {

    val target = I + I
    val domain = shape*shape

    override def toString():String = s"equal($shape)"

    def serialize: Iterable[Byte] = Iterable(v2.Morphism.constants.equal) ++ shape.serialize

    def apply(x: (A, A)): BOOL = x._1 == x._2

  }

  def equals[A](shape:Shape[A],a:A):Morphism[A,BOOL] = equal[A](shape) <<[A] product.leftPairedWith[A,A](shape,shape,a)

  object and extends Morphism[(BOOL,BOOL),BOOL] {

    val target = I + I
    val domain = target*target

    override def toString():String = s"and"

    def serialize: Iterable[Byte] = Iterable(v2.Morphism.constants.and)

    def apply(x:(BOOL,BOOL)):BOOL = x._1 && x._2

  }

  object or extends Morphism[(BOOL,BOOL),BOOL] {

    val target = I + I
    val domain = target*target

    override def toString():String = s"or"

    def serialize: Iterable[Byte] = Iterable(v2.Morphism.constants.or)

    def apply(x:(BOOL,BOOL)):BOOL = x._1 || x._2

  }

  object xor extends Morphism[(BOOL,BOOL),BOOL] {

    val target = I + I
    val domain = target*target

    override def toString():String = s"xor"

    def serialize: Iterable[Byte] = Iterable(v2.Morphism.constants.xor)

    def apply(x:(BOOL,BOOL)):BOOL = x._1 != x._2

  }

  val not:Isomorphism[BOOL,BOOL] = sum.commute(I,I)


}
