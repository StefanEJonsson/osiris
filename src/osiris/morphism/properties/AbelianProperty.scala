// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.morphism.properties

import osiris._
import osiris.shape.Shape
import morphism.Isomorphism

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
      code ++ Iterable(utilities.Serialization.Morphism.commute) ++ a.serialize ++ b.serialize

    override def toString():String = s"${AbelianProperty.this}.commute($a,$b)"

  }

}
