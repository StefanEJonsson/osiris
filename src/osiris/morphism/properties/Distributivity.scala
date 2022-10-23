// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.morphism.properties

import osiris._
import osiris.shape.Shape
import morphism.Isomorphism
import osiris.utilities.serialization.v2

/**
  * Contains the Isomorphisms leftDistr, rightDistr, leftExtract and rightExtract.
  *
  * leftDistr and leftExtract are each others inverses. rightDistr and rightExtract are each others inverses.
  */
trait Distributivity[F[_,_],G[_,_]] {

  protected def F[A,B](a:Shape[A], b:Shape[B]):Shape[F[A,B]]

  protected def G[A,B](a:Shape[A], b:Shape[B]):Shape[G[A,B]]

  protected def ld[A,B,C](x:F[A,G[B,C]]):G[F[A,B],F[A,C]]

  protected def rd[A,B,C](x:F[G[A,B],C]):G[F[A,C],F[B,C]]

  protected def le[A,B,C](x:G[F[A,B],F[A,C]]):F[A,G[B,C]]

  protected def re[A,B,C](x:G[F[A,C],F[B,C]]):F[G[A,B],C]

  def leftDistr[A,B,C](a:Shape[A],b:Shape[B],c:Shape[C]) = new Isomorphism[F[A,G[B,C]],G[F[A,B],F[A,C]]] {

    val domain = F(a,G(b,c))
    val target = G(F(a,b),F(a,c))

    lazy val inverse = leftExtract(a,b,c)

    def apply(x:F[A,G[B,C]]):G[F[A,B],F[A,C]] = ld(x)

    def serialize:Iterable[Byte] =
      Iterable(v2.Morphism.constants.distrLeft) ++ a.serialize ++ b.serialize ++ c.serialize

    override def toString():String = s"leftDistr($a,$b,$c)"

  }

  def rightDistr[A,B,C](a:Shape[A],b:Shape[B],c:Shape[C]) = new Isomorphism[F[G[A,B],C],G[F[A,C],F[B,C]]] {

    val domain = F(G(a,b),c)
    val target = G(F(a,c),F(b,c))

    lazy val inverse = rightExtract(a,b,c)

    def apply(x:F[G[A,B],C]):G[F[A,C],F[B,C]] = rd(x)

    def serialize:Iterable[Byte] =
      Iterable(v2.Morphism.constants.distrRight) ++ a.serialize ++ b.serialize ++ c.serialize

    override def toString():String = s"rightDistr($a,$b,$c)"

  }

  def leftExtract[A,B,C](a:Shape[A],b:Shape[B],c:Shape[C]):Isomorphism[G[F[A,B],F[A,C]],F[A,G[B,C]]] =
    new Isomorphism[G[F[A,B],F[A,C]],F[A,G[B,C]]] {

      val domain = G(F(a,b),F(a,c))
      val target = F(a,G(b,c))

      lazy val inverse = leftDistr(a,b,c)

      def apply(x:G[F[A,B],F[A,C]]):F[A,G[B,C]] = le(x)

      def serialize:Iterable[Byte] =
        Iterable(v2.Morphism.constants.extractLeft) ++ a.serialize ++ b.serialize ++ c.serialize

      override def toString():String = s"leftExtract($a,$b,$c)"

    }

  def rightExtract[A,B,C](a:Shape[A],b:Shape[B],c:Shape[C]):Isomorphism[G[F[A,C],F[B,C]],F[G[A,B],C]] =
    new Isomorphism[G[F[A,C],F[B,C]],F[G[A,B],C]] {

      val domain = G(F(a,c),F(b,c))
      val target = F(G(a,b),c)

      lazy val inverse = rightDistr(a,b,c)

      def apply(x:G[F[A,C],F[B,C]]):F[G[A,B],C] = re(x)

      def serialize:Iterable[Byte] =
        Iterable(v2.Morphism.constants.extractRight) ++ a.serialize ++ b.serialize ++ c.serialize

      override def toString():String = s"rightExtract($a,$b,$c)"

    }

}