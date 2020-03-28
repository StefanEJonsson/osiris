// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.morphism

import osiris._
import shape._
import osiris.utilities.serialization.v2

/**
  * Function with a finite domain and finite target.
  *
  * Morphisms are used to represent functions on indices of vectors. They are important when doing reindexing operations
  * on vectors. A reindexing operation takes a vector and produces a new vector containing the same elements but with a
  * different shape and with some elements possibly omitted, some elements possibly duplicated and the remaining
  * elements possibly in a different order. In this context the morphism is used to describe the relation between the
  * index of an element in the new vector and its index in the old vector.
  *
  * @tparam A input type
  * @tparam B output type
  */
trait Morphism[A,B] extends (A => B) {

  val domain:Shape[A]
  val target:Shape[B]

  def serialize:Iterable[Byte]

  /**
    * Function composition.
    */
  def <<[A0](that:Morphism[A0,A]):Morphism[A0,B] = new CompositeMorphism(this,that)

}

/**
  * Composition of two Morphisms.
  */
class CompositeMorphism[A,B,C](f:Morphism[B,C], g:Morphism[A,B]) extends Morphism[A,C] {

  val domain = g.domain
  val target = f.target

  def apply(x:A):C = f(g(x))

  override def toString():String = s"($f << $g)"

  def serialize:Iterable[Byte] = Iterable(v2.Morphism.constants.composition) ++ f.serialize ++ g.serialize

}

/**
  * A morphism implemented as a simple table of input-output mappings.
  */
class TabularMorphism[A,B](val domain:Shape[A],val target:Shape[B],map:container.Container[A,B]) extends Morphism[A,B] {

  def serialize:Iterable[Byte] =
    Iterable(v2.Morphism.constants.table) ++
      domain.serialize ++ target.serialize ++ domain.iterator.flatMap(a => utilities.serialization.v2.Index.serializeIndex(map(a)))

  def apply(a:A):B = map(a)

}

/**
  * Morphisms with the with the injective (one-to-one) property.
  *
  * These are used to represent extractions. Extractions are reindexing operations the do not duplicate any elements.
  */
trait Monomorphism[A,B] extends Morphism[A,B] {

  def o[A0](that:Monomorphism[A0,A]):Monomorphism[A0,B] = new CompositeMonomorphism(this,that)

  /**
    * The pseudo inverse of the monomorphism. For every element b in B, there is either going to be one unique element
    * a in A that maps to b or no element in A maps to b. The monoinverse returns Left(a) if such an element exists and
    * Right(()) otherwise.
    */
  val monoInverse:Morphism[B,Either[A,Unit]]

}

class CompositeMonomorphism[A,B,C](f: Monomorphism[B, C], g: Monomorphism[A, B])
  extends CompositeMorphism(f,g) with Monomorphism[A,C] {

  override def toString():String = s"($f << $g)"

  lazy val monoInverse:Morphism[C,Either[A,Unit]] =
    sum.rmap(g.domain,sum.join(I))  <<
    sum.assocRight(g.domain,I,I)    <<
    sum.lmap(g.monoInverse,I)       <<
    f.monoInverse

}

/**
  * Morphisms with the bijective (one-to-one and onto) property.
  *
  * These morphisms are used to represent permutations. Permutations are reindexing operations that do not discard or
  * duplicate any elements. Every element in the input vector will occur exactly once in the output vector. Note that
  * this notion of a permutation deviates slightly from the one normally used in mathematics. In mathematics, a
  * permutation is a bijection from a set to itself. Here, the shape of the vector is allowed to change, as long as the
  * morphism is bijective.
  */
trait Isomorphism[A,B] extends Monomorphism[A,B] {

  val inverse:Isomorphism[B,A]

  lazy val monoInverse = sum.left(domain,I) o inverse

  def o[A0](g: Isomorphism[A0,A]):Isomorphism[A0,B] = new CompositeIsomorphism[A0,A,B](this,g)

}

class CompositeIsomorphism[A,B,C](f:Isomorphism[B,C], g:Isomorphism[A,B])
  extends CompositeMorphism(f,g) with Isomorphism[A,C] {

  lazy val inverse:Isomorphism[C,A] = g.inverse o f.inverse

  override def toString():String = s"($f << $g)"

}