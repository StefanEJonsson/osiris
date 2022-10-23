// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.morphism

import osiris._
import osiris.shape.Shape
import morphism.properties.{AbelianProperty, Bifunctor, Monoidicity}
import osiris.utilities.serialization.v2

/**
  * Contains some of the important morphisms associated with the Either type.
  */
object sum extends Bifunctor[+] with Monoidicity[+,Nothing] with AbelianProperty[+] {

  protected val i = O

  protected def F[A,B](a:Shape[A], b:Shape[B]):Shape[+[A,B]] = a + b

  protected def code = Iterable(v2.Morphism.constants.sum)

  override def toString():String = "sum"

  //bifunctoriality

  def bim[L1,L2,R1,R2](l:Morphism[L1,L2],r:Morphism[R1,R2]): Morphism[+[L1,R1],+[L2,R2]] =
    new Morphism[Either[L1,R1],Either[L2,R2]] {

      val domain = l.domain + r.domain
      val target = l.target + r.target

      override def toString():String = s"${sum.this}.bimap($l,$r)"

      def serialize: Iterable[Byte] =
        Iterable(v2.Morphism.constants.sum,v2.Morphism.constants.bimap) ++
        l.serialize ++ r.serialize

      def apply(i:Either[L1,R1]):Either[L2,R2] = i match {
        case Left(x) => Left(l(x))
        case Right(x) => Right(r(x))
      }

    }


  //monoidicity

  protected def gl[A](x:Either[A,Nothing]) = x.left.get

  protected def gr[A](x:Either[Nothing,A]):A = x.right.get

  protected def pl[A](x:A):Either[A,Nothing] = Left(x)

  protected def pr[A](x:A):Either[Nothing,A] = Right(x)

  protected def al[A,B,C](x:Either[A,Either[B,C]]):Either[Either[A,B],C] = x match {
    case  Left(x)        =>  Left( Left(x))
    case Right( Left(x)) =>  Left(Right(x))
    case Right(Right(x)) => Right(x)
  }

  protected def ar[A,B,C](x:Either[Either[A,B],C]):Either[A,Either[B,C]] = x match {
    case  Left( Left(x)) =>  Left(x)
    case  Left(Right(x)) => Right( Left(x))
    case Right(x)        => Right(Right(x))
  }

  protected def commutation[A,B](x:Either[A,B]):Either[B,A] = x.swap

  //epimorphisms

  /**
    * Takes an element of type Either[A,A] and produces an element of type A, by simply discarding the Left/Right tag.
    */
  def join[A](shape:Shape[A]) = new Morphism[+[A,A],A] {

    val domain = shape + shape
    val target = shape

    override def toString():String = s"join($shape)"

    def apply(x:Either[A,A]):A = x match {
      case  Left(x) => x
      case Right(x) => x
    }

    def serialize: Iterable[Byte] =
      Iterable(v2.Morphism.constants.join) ++ shape.serialize

  }

  //monomorphisms

  /**
    * Takes an element of type A and produces an element of type Either[A,B] by giving it a Left-tag.
    *
    * x => Left(x)
    */
  def left[A,B](l:Shape[A],r:Shape[B]) = new Monomorphism[A,+[A,B]] {

    val domain = l
    val target = l+r

    lazy val monoInverse:Morphism[+[A,B],+[A,Unit]] = sum.rmap(l,unit(r))

    def apply(x:A):Either[A,B] = Left(x)

    def serialize: Iterable[Byte] = Iterable(v2.Morphism.constants.left) ++ l.serialize ++ r.serialize

    override def toString():String = s"sum.left($l,$r)"

  }

  /**
    * Takes an element of type B and produces an element of type Either[A,B] by giving it a Right-tag.
    *
    * x => Right(x)
    */
  def right[A,B](l:Shape[A],r:Shape[B]) = new Monomorphism[B,+[A,B]] {

    val domain = r
    val target = l+r

    lazy val monoInverse:Morphism[Either[A,B],Either[B,Unit]] = sum.rmap(r,unit[A](l)) << sum.commute(l,r)

    def apply(x:B):Either[A,B] = Right(x)

    def serialize: Iterable[Byte] = Iterable(v2.Morphism.constants.right) ++ l.serialize ++ r.serialize

    override def toString():String = s"sum.right($l,$r)"

  }

  /**
    * Computes the pseudo-inverse of sum.lmap(f) given that f is a MonoMorphism.
    */
  def lMapMonoInverse[L1,L2,R](f:Monomorphism[L1,L2], r:Shape[R]):Morphism[+[L2,R],+[+[L1,R],Unit]] =
    commute(I,f.domain+r) <<
    assocRight(I,f.domain,r) <<
    lm(commute(f.domain,I),r) <<
    lm(f.monoInverse,r)

  /**
    * Computes the pseudo-inverse of sum.rmap(f) given that f is a MonoMorphism.
    */
  def rMapMonoInverse[L,R1,R2](l:Shape[L],f:Monomorphism[R1,R2]) =
    assocLeft(l,f.domain,I) <<
    rm[L,R2,+[R1,Unit]](l,f.monoInverse)

  /**
    * Computes the pseudo-inverse of sum.bimap(l,r) given that l and r are MonoMorphisms.
    */
  def biMapMonoInverse[L1,L2,R1,R2](l:Monomorphism[L1,L2], r:Monomorphism[R1,R2]) =
    rm(l.domain + r.domain, join(I)) <<
    assocRight(l.domain+r.domain,I,I) <<
    lm(assocLeft(l.domain,r.domain,I),I) <<
    lm(rm(l.domain,commute(I,r.domain)),I) <<
    lm(assocRight(l.domain,I,r.domain),I) <<
    assocLeft(l.domain+I,r.domain,I) <<
    bim(l.monoInverse,r.monoInverse)

}
