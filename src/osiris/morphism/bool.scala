// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.morphism

import osiris._
import osiris.shape.Shape

object bool {

  //TODO feels like more predicates should be needed?

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

    def serialize: Iterable[Byte] = Iterable(utilities.Serialization.Morphism.equal) ++ shape.serialize

    def apply(x: (A, A)): BOOL = x._1 == x._2

  }

  def equals[A](shape:Shape[A],a:A):Morphism[A,BOOL] = equal[A](shape) <<[A] product.leftPairedWith[A,A](shape,shape,a)

  object and extends Morphism[(BOOL,BOOL),BOOL] {

    val target = I + I
    val domain = target*target

    def serialize: Iterable[Byte] = Iterable(utilities.Serialization.Morphism.and)

    def apply(x:(BOOL,BOOL)):BOOL = x._1 && x._2

  }

  object or extends Morphism[(BOOL,BOOL),BOOL] {

    val target = I + I
    val domain = target*target

    def serialize: Iterable[Byte] = Iterable(utilities.Serialization.Morphism.or)

    def apply(x:(BOOL,BOOL)):BOOL = x._1 || x._2

  }

  object xor extends Morphism[(BOOL,BOOL),BOOL] {

    val target = I + I
    val domain = target*target

    def serialize: Iterable[Byte] = Iterable(utilities.Serialization.Morphism.xor)

    def apply(x:(BOOL,BOOL)):BOOL = x._1 != x._2

  }

  val not:Isomorphism[BOOL,BOOL] = sum.commute(I,I)


}
