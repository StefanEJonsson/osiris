// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris

import osiris.shape.Shape
import osiris.utilities.serialization
import osiris.utilities.serialization.v2
import osiris.utilities.serialization.Serialization

package object morphism {

  /**
    * The identity morphism. x => x
    */
  def id[A](shape:Shape[A]) = new Isomorphism[A,A] {

    val domain = shape
    val target = shape

    val inverse = this

    override def toString():String = s"id($shape)"

    def apply(x:A):A = x

    def serialize:Iterable[Byte] = Iterable(v2.Morphism.constants.id) ++ shape.serialize

  }

  /**
    * Takes any element of A and always returns ()
    */
  def unit[A](shape:Shape[A]) = new Morphism[A,Unit] {

    val domain = shape
    val target = I

    override def toString():String = s"unit($shape)"

    def apply(x:A):Unit = ()

    def serialize:Iterable[Byte] = Iterable(v2.Morphism.constants.unit) ++ shape.serialize

  }

  /**
    * The empty function.
    */
  def absurd[A](shape:Shape[A]) = new Monomorphism[Nothing,A] {

    val domain = O
    val target = shape

    lazy val monoInverse:Morphism[A,Either[Nothing,Unit]] = sum.right[Nothing,Unit](O,I) << unit(shape)

    override def toString():String = s"absurd($shape)"

    def apply(x:Nothing):A = utilities.absurd(x)

    def serialize:Iterable[Byte] = Iterable(v2.Morphism.constants.absurd) ++ shape.serialize

  }

  /**
    * Takes () as input and always returns the constant a.
    */
  def constant[A](shape:Shape[A],a:A) = new Monomorphism[Unit,A] {

    val domain = I
    val target = shape

    lazy val monoInverse:Morphism[A,Either[Unit,Unit]] = bool.equals(shape,a)

    override def toString():String = s"constant($shape,$a)"

    def apply(x:Unit):A = a

    def serialize:Iterable[Byte] =
      Iterable(v2.Morphism.constants.constant) ++ shape.serialize ++ utilities.serialization.v2.Index.serializeIndex(a)

  }


}
