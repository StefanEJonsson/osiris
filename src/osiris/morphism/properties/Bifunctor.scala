// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.morphism.properties

import osiris._
import osiris.shape.Shape
import morphism.{Isomorphism, Monomorphism, Morphism}
import osiris.utilities.serialization
import osiris.utilities.serialization.v2
import osiris.utilities.serialization.Serialization

/**
  * Contains the lmap, rmap and bimap functions.
  *
  * lmap takes a function A --> B and produces a function F[A,C] --> F[B,C].
  * rmap function takes a function A --> B and produces a function F[C,A] --> F[C,B].
  * bimap takes two functions (A --> B and C --> D) and produces a function F[A,C] --> F[B,D].
  *
  * There are a few constraints on the implementations on lmap, rmap and bimap that need to be met:
  *
  * lmap(f) = bimap(f,id), rmap(f) = bimap(id,f), bimap(f,g) = lmap(f) << rmap(g) = rmap(g) << lmap(f)
  * lmap(id) = id, rmap(id) = id, bimap(id,id) = id
  * lmap(f << g) = lmap(f) << lmap(g), rmap(f << g) = rmap(f) << map(g)
  *
  * Tuple2 is an example of a Bifunctor. In this case lmap(f) has the implementation:
  * (x,y) => (f(x),y)
  * rmap(f) can similarly be implemented:
  * (x,y) => (x,f(y))
  * and bimap(f,g):
  * (x,y) => (f(x),g(y))
  *
  */
trait Bifunctor[F[_,_]] {

  protected def F[A,B](a:Shape[A],b:Shape[B]):Shape[F[A,B]]

  protected def code:Iterable[Byte]

  protected def lm[L1,L2,R](f:Morphism[L1,L2],r:Shape[R]):Morphism[F[L1,R],F[L2,R]] = bim(f,morphism.id(r))

  protected def rm[L,R1,R2](l:Shape[L],f:Morphism[R1,R2]):Morphism[F[L,R1],F[L,R2]] = bim(morphism.id(l),f)

  protected def bim[L1,L2,R1,R2](l:Morphism[L1,L2],r:Morphism[R1,R2]):Morphism[F[L1,R1], F[L2,R2]]


  protected def lMapMonoInverse[L1,L2,R](f:Monomorphism[L1,L2], r:Shape[R]):Morphism[F[L2,R],Either[F[L1,R],Unit]]

  protected def rMapMonoInverse[L,R1,R2](l:Shape[L],f:Monomorphism[R1,R2]):Morphism[F[L,R2],Either[F[L,R1],Unit]]

  protected def biMapMonoInverse[L1,L2,R1,R2](l:Monomorphism[L1,L2],
                                              r:Monomorphism[R1,R2]):Morphism[F[L2,R2],Either[F[L1,R1],Unit]]

  def lmap[L1,L2,R](f:Morphism[L1, L2],r:Shape[R]):Morphism[F[L1,R],F[L2,R]] = lm(f,r)

  def rmap[L,R1,R2](l:Shape[L],f:Morphism[R1,R2]):Morphism[F[L,R1],F[L,R2]] = rm(l,f)

  def bimap[L1,L2,R1,R2](l:Morphism[L1,L2],r:Morphism[R1,R2]):Morphism[F[L1,R1],F[L2,R2]] = bim(l,r)


  //preservation of isicity under map operations

  def lmap[L1,L2,R](f:Isomorphism[L1,L2],r:Shape[R]):Isomorphism[F[L1,R],F[L2,R]] = new Isomorphism[F[L1,R],F[L2,R]] {

    val domain = F(f.domain,r)
    val target = F(f.target,r)

    lazy val inverse = lmap(f.inverse,r)

    def apply(x:F[L1,R]):F[L2,R] = lm[L1,L2,R](f,r)(x)

    def serialize:Iterable[Byte] = code ++ Iterable(v2.Morphism.constants.lmap) ++ f.serialize ++ r.serialize

    override def toString():String = s"${Bifunctor.this}.lmap($f,$r)"

  }

  def rmap[L,R1,R2](l:Shape[L],f:Isomorphism[R1,R2]):Isomorphism[F[L,R1],F[L,R2]] = new Isomorphism[F[L,R1],F[L,R2]] {

    val domain = F(l,f.domain)
    val target = F(l,f.target)

    lazy val inverse = rmap(l,f.inverse)

    def apply(x:F[L,R1]):F[L,R2] = rm[L,R1,R2](l,f)(x)

    def serialize:Iterable[Byte] = code ++ Iterable(v2.Morphism.constants.rmap) ++ l.serialize ++ f.serialize

    override def toString():String = s"${Bifunctor.this}.rmap($l,$f)"

  }

  def bimap[L1,L2,R1,R2](l:Isomorphism[L1,L2],r:Isomorphism[R1,R2]):Isomorphism[F[L1,R1],F[L2,R2]] =
    new Isomorphism[F[L1,R1],F[L2,R2]] {

      val domain = F(l.domain,r.domain)
      val target = F(l.target,r.target)

      lazy val inverse = bimap(l.inverse,r.inverse)

      def apply(x:F[L1,R1]):F[L2,R2] = bim(l,r)(x)

      def serialize:Iterable[Byte] =
        code ++ Iterable(v2.Morphism.constants.bimap) ++ l.serialize ++ r.serialize

      override def toString():String = s"${Bifunctor.this}.bimap($l,$r)"

    }

  //preservation of monicity under map operations

  def lmap[L1,L2,R](f:Monomorphism[L1,L2], r:Shape[R]):Monomorphism[F[L1,R],F[L2,R]] =
    new Monomorphism[F[L1,R],F[L2,R]] {

      val domain = F(f.domain, r)
      val target = F(f.target, r)

      lazy val monoInverse:Morphism[F[L2,R],Either[F[L1,R],Unit]] = lMapMonoInverse(f,r)

      def apply(x:F[L1,R]):F[L2,R] = lm[L1,L2,R](f,r)(x)

      def serialize:Iterable[Byte] =
        code ++ Iterable(v2.Morphism.constants.lmap) ++ f.serialize ++ r.serialize

      override def toString():String = s"${Bifunctor.this}.lmap($f,$r)"

    }

  def rmap[L,R1,R2](l:Shape[L],f:Monomorphism[R1,R2]):Monomorphism[F[L,R1],F[L,R2]] =
    new Monomorphism[F[L,R1],F[L,R2]] {

      val domain = F(l,f.domain)
      val target = F(l,f.target)

      lazy val monoInverse = rMapMonoInverse(l,f)

      def apply(x:F[L,R1]):F[L,R2] = rm[L,R1,R2](l,f)(x)

      def serialize:Iterable[Byte] =
        code ++ Iterable(v2.Morphism.constants.rmap) ++ l.serialize ++ f.serialize

      override def toString():String = s"${Bifunctor.this}.lmap($l,$f)"

    }

  def bimap[L1,L2,R1,R2](l:Monomorphism[L1,L2], r:Monomorphism[R1,R2]):Monomorphism[F[L1,R1],F[L2,R2]] =
    new Monomorphism[F[L1,R1],F[L2,R2]] {

      val domain = F(l.domain, r.domain)
      val target = F(l.target,r.target)

      lazy val monoInverse = biMapMonoInverse(l,r)

      def apply(x: F[L1,R1]): F[L2,R2] = bim(l,r)(x)

      def serialize:Iterable[Byte] =
        code ++ Iterable(v2.Morphism.constants.bimap) ++ l.serialize ++ r.serialize

      override def toString():String = s"$this.lmap($l,$r)"

    }


}