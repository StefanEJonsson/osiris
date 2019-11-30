// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris

import osiris.shape.Shape

package object morphism {

  def id[A](shape:Shape[A]) = new Isomorphism[A,A] {

    val domain = shape
    val target = shape

    val inverse = this

    def apply(x:A):A = x

    def serialize:Iterable[Byte] = Iterable(utilities.Serialization.Morphism.id) ++ shape.serialize

  }

  def unit[A](shape:Shape[A]) = new Morphism[A,Unit] {

    val domain = shape
    val target = I

    def apply(x:A):Unit = ()

    def serialize:Iterable[Byte] = Iterable(utilities.Serialization.Morphism.unit) ++ shape.serialize

  }

  def absurd[A](shape:Shape[A]) = new Monomorphism[Nothing,A] {

    val domain = O
    val target = shape

    lazy val monoInverse:Morphism[A,Either[Nothing,Unit]] = sum.right[Nothing,Unit](O,I) << unit(shape)

    def apply(x:Nothing):A = utilities.absurd(x)

    def serialize:Iterable[Byte] = Iterable(utilities.Serialization.Morphism.absurd) ++ shape.serialize

  }


  def constant[A](shape:Shape[A],a:A) = new Monomorphism[Unit,A] {

    val domain = I
    val target = shape

    lazy val monoInverse:Morphism[A,Either[Unit,Unit]] = bool.equals(shape,a)

    def apply(x:Unit):A = a

    def serialize:Iterable[Byte] =
      Iterable(utilities.Serialization.Morphism.constant) ++ shape.serialize ++ Shape.serializeIndex(a)

  }


}
