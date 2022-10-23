// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.morphism.properties

import osiris._
import osiris.shape.Shape
import morphism.Isomorphism
import osiris.utilities.serialization.v2

/**
  * Contains the commute function. The commute function is an isomorphism between F[A,B] and F[B,A] where F is some
  * generic type with two type parameters.
  *
  * An example of a generic type that has the Abelian property is Tuple2. The function that takes a tuple and produces
  * a new tuple by simply swapping the elements ((x,y) => (y,x)) is a function from Tuple2[A,B] to Tuple2[B,A].
  * This function is its own inverse. Because it has an inverse it counts as an Isomorphism and can be used as an
  * implementation of the commute function for Tuple2, proving that Tuple2 has the Abelian property.
  */
trait AbelianProperty[F[_,_]] {

  protected def F[A,B](a:Shape[A],b:Shape[B]):Shape[F[A,B]]

  protected def code:Iterable[Byte]

  protected def commutation[A,B](x:F[A,B]):F[B,A]

  def commute[A,B](a:Shape[A],b:Shape[B]):Isomorphism[F[A,B],F[B,A]] = new Isomorphism[F[A,B],F[B,A]] {

    val domain = F(a,b)
    val target = F(b,a)

    lazy val inverse = commute(b,a)

    def apply(x:F[A,B]):F[B,A] = commutation(x)

    def serialize:Iterable[Byte] =
      code ++ Iterable(v2.Morphism.constants.commute) ++ a.serialize ++ b.serialize

    override def toString():String = s"${AbelianProperty.this}.commute($a,$b)"

  }

}
