// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.morphism.properties

import osiris._
import osiris.shape.Shape
import morphism.Isomorphism

trait Monoidicity[F[_,_],Identity] {

  protected def F[A,B](a:Shape[A], b:Shape[B]):Shape[F[A,B]]

  protected val i:Shape[Identity]

  protected def code:Iterable[Byte]

  protected def gl[A](x:F[A,Identity]):A

  protected def gr[A](x:F[Identity,A]):A

  protected def pl[A](x:A):F[A,Identity]

  protected def pr[A](x:A):F[Identity,A]

  protected def al[A,B,C](x:F[A,F[B,C]]):F[F[A,B],C]

  protected def ar[A,B,C](x:F[F[A,B],C]):F[A,F[B,C]]

  def getLeft[A](shape:Shape[A]) = new Isomorphism[F[A,Identity],A] {

    val domain = F(shape,i)
    val target = shape

    lazy val inverse = putLeft(shape)

    def apply(x:F[A,Identity]):A = gl(x)

    def serialize:Iterable[Byte] =
      code ++ Iterable(utilities.Serialization.Morphism.getLeft) ++ shape.serialize

  }

  def getRight[A](shape:Shape[A]) = new Isomorphism[F[Identity,A],A] {

    val domain = F(i,shape)
    val target = shape

    lazy val inverse = putRight(shape)

    def apply(x:F[Identity,A]):A = gr(x)

    def serialize:Iterable[Byte] =
      code ++ Iterable(utilities.Serialization.Morphism.getRight) ++ shape.serialize

  }

  def putLeft[A](shape:Shape[A]):Isomorphism[A,F[A,Identity]] = new Isomorphism[A,F[A,Identity]] {

    val domain = shape
    val target = F(shape,i)

    lazy val inverse = getLeft(shape)

    def apply(x:A):F[A,Identity] = pl(x)

    def serialize:Iterable[Byte] =
      code ++ Iterable(utilities.Serialization.Morphism.putLeft) ++ shape.serialize

  }

  def putRight[A](shape:Shape[A]):Isomorphism[A,F[Identity,A]] = new Isomorphism[A,F[Identity,A]] {

    val domain = shape
    val target = F(i,shape)

    lazy val inverse = getRight(shape)

    def apply(x:A):F[Identity,A] = pr(x)

    def serialize:Iterable[Byte] =
      code ++ Iterable(utilities.Serialization.Morphism.putRight) ++ shape.serialize

  }

  def assocLeft[A,B,C](a:Shape[A],b:Shape[B],c:Shape[C]) = new Isomorphism[F[A,F[B,C]],F[F[A,B],C]] {

    val domain = F(a,F(b,c))
    val target = F(F(a,b),c)

    lazy val inverse = assocRight(a,b,c)

    def apply(x:F[A,F[B,C]]):F[F[A,B],C] = al(x)

    def serialize:Iterable[Byte] =
      code ++ Iterable(utilities.Serialization.Morphism.leftAssoc) ++ a.serialize ++ b.serialize ++ c.serialize

  }

  def assocRight[A,B,C](a:Shape[A],b:Shape[B],c:Shape[C]):Isomorphism[F[F[A,B],C],F[A,F[B,C]]] =
    new Isomorphism[F[F[A,B],C],F[A,F[B,C]]] {

    val domain = F(F(a,b),c)
    val target = F(a,F(b,c))

    lazy val inverse = assocLeft(a,b,c)

    def apply(x:F[F[A,B],C]):F[A,F[B,C]] = ar(x)

    def serialize:Iterable[Byte] =
      code ++ Iterable(utilities.Serialization.Morphism.rightAssoc) ++ a.serialize ++ b.serialize ++ c.serialize

  }

}