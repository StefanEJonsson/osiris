// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.morphism

import osiris._
import shape._
import Shape.serializeIndex


trait Morphism[A,B] extends (A => B) {

  val domain:Shape[A]
  val target:Shape[B]

  def serialize:Iterable[Byte]

  def <<[A0](that:Morphism[A0,A]):Morphism[A0,B] = new CompositeMorphism(this,that)

}

object Morphism {

  import utilities.Serialization.{Morphism => M}

  def deserialize(bytes:Iterator[Byte]):Morphism[_,_] = {
    val h = bytes.next()
    h match {
      case M.composition => {
        val f = deserialize(bytes)
        val g = deserialize(bytes)
        type A = g.domain.Type
        type B = g.target.Type
        type C = f.target.Type
        f.asInstanceOf[Morphism[B,C]] << g.asInstanceOf[Morphism[A,B]]
      }
      case M.sum     => deserializeMonoidalTransformation[+,Nothing](sum,bytes)
      case M.product => deserializeMonoidalTransformation[*,Unit](product,bytes)

      case M.id => {
        val shape = Shape.deserialize(bytes)
        id[shape.Type](shape.asInstanceOf[Shape[shape.Type]]) //TODO seems unnecessarily complicated
      }
      case M.unit => {
        val shape = Shape.deserialize(bytes)
        unit(shape)
      }
      case M.absurd => {
        val shape = Shape.deserialize(bytes)
        absurd(shape)
      }
      case M.constant => {
        val shape = Shape.deserialize(bytes)
        val idx:shape.Type = shape.deserializeIndex(bytes).asInstanceOf[shape.Type]
        constant(shape.asInstanceOf[Shape[shape.Type]],idx)
      }
      case M.table => {
        val domain = Shape.deserialize(bytes)
        val target = Shape.deserialize(bytes)
        type D = domain.Type
        type T = target.Type
        val d = domain.asInstanceOf[Shape[D]]
        val t = target.asInstanceOf[Shape[T]]
        val map = (d -->[T]())((_:D) => t.deserializeIndex(bytes))
        new TabularMorphism[D,T](d,t,map)
      }
      case M.translate => {
         val range = Shape.deserialize(bytes).asInstanceOf[Range]
         val dx = utilities.Serialization.Primitives.deserializeInt(bytes)
         int.translate(dx,range)
      }
      case M.inv => {
        val range = Shape.deserialize(bytes).asInstanceOf[Range]
        int.inv(range)
      }

      case M.equal => {
        val shape = Shape.deserialize(bytes)
        bool.equal(shape)
      }
      case M.and => bool.and
      case M.or => bool.or
      case M.xor => bool.xor

      case M.left => {
        val l = Shape.deserialize(bytes)
        val r = Shape.deserialize(bytes)
        sum.left(l,r)
      }
      case M.right => {
        val l = Shape.deserialize(bytes)
        val r = Shape.deserialize(bytes)
        sum.right(l,r)
      }
      case M.join => {
        val shape = Shape.deserialize(bytes)
        sum.join(shape)
      }

      case M.distrLeft => {
        val a = Shape.deserialize(bytes)
        val b = Shape.deserialize(bytes)
        val c = Shape.deserialize(bytes)
        product.leftDistr(a,b,c)
      }
      case M.distrRight => {
        val a = Shape.deserialize(bytes)
        val b = Shape.deserialize(bytes)
        val c = Shape.deserialize(bytes)
        product.rightDistr(a,b,c)
      }
      case M.extractLeft => {
        val a = Shape.deserialize(bytes)
        val b = Shape.deserialize(bytes)
        val c = Shape.deserialize(bytes)
        product.leftExtract(a,b,c)
      }
      case M.extractRight => {
        val a = Shape.deserialize(bytes)
        val b = Shape.deserialize(bytes)
        val c = Shape.deserialize(bytes)
        product.rightExtract(a,b,c)
      }

      case M.first => {
        val fst = Shape.deserialize(bytes)
        val snd = Shape.deserialize(bytes)
        product.first(fst,snd)
      }
      case M.second => {
        val fst = Shape.deserialize(bytes)
        val snd = Shape.deserialize(bytes)
        product.second(fst,snd)
      }
      case M.firstWith => {
        val fst = Shape.deserialize(bytes)
        val snd = Shape.deserialize(bytes)
        val c:snd.Type = snd.deserializeIndex(bytes).asInstanceOf[snd.Type]
        product.leftPairedWith(fst.asInstanceOf[Shape[fst.Type]],snd.asInstanceOf[Shape[snd.Type]],c)
      }
      case M.secondWith => {
        val fst = Shape.deserialize(bytes)
        val snd = Shape.deserialize(bytes)
        val c:fst.Type = fst.deserializeIndex(bytes).asInstanceOf[fst.Type]
        product.rightPairedWith(fst.asInstanceOf[Shape[fst.Type]],snd.asInstanceOf[Shape[snd.Type]],c)
      }

      case M.spookLeft => {
        val shape = Shape.deserialize(bytes)
        product.spookLeft(shape)
      }
      case M.spookRight => {
        val shape = Shape.deserialize(bytes)
        product.spookRight(shape)
      }
      case M.bustLeft => {
        val shape = Shape.deserialize(bytes)
        product.bustLeft(shape)
      }
      case M.bustRight => {
        val shape = Shape.deserialize(bytes)
        product.bustRight(shape)
      }

      case M.copy => {
        val shape = Shape.deserialize(bytes)
        product.copy(shape)
      }
    }
  }

  private type AbelianMonoid[F[_,_],I] = properties.Bifunctor[F] with
    properties.Monoidicity[F,I] with properties.AbelianProperty[F]

  private def deserializeMonoidalTransformation[F[_,_],I](monoid:AbelianMonoid[F,I],
                                                              bytes:Iterator[Byte]):Morphism[_,_] = {
    val h = bytes.next()
    h match {
      case M.bimap => {
        val a = deserialize(bytes)
        val b = deserialize(bytes)
        monoid.bimap(a,b)
      }
      case M.lmap => {
        val a = deserialize(bytes)
        val r = Shape.deserialize(bytes)
        monoid.lmap(a,r)
      }
      case M.rmap => {
        val l = Shape.deserialize(bytes)
        val b = deserialize(bytes)
        monoid.rmap(l,b)
      }
      case M.commute    => {
        val a = Shape.deserialize(bytes)
        val b = Shape.deserialize(bytes)
        monoid.commute(a,b)
      }
      case M.leftAssoc  => {
        val a = Shape.deserialize(bytes)
        val b = Shape.deserialize(bytes)
        val c = Shape.deserialize(bytes)
        monoid.assocLeft(a,b,c)
      }
      case M.rightAssoc => {
        val a = Shape.deserialize(bytes)
        val b = Shape.deserialize(bytes)
        val c = Shape.deserialize(bytes)
        monoid.assocRight(a,b,c)
      }
      case M.getLeft    => {
        val shape = Shape.deserialize(bytes)
        monoid.getLeft(shape)
      }
      case M.getRight   => {
        val shape = Shape.deserialize(bytes)
        monoid.getRight(shape)
      }
      case M.putLeft    => {
        val shape = Shape.deserialize(bytes)
        monoid.putLeft(shape)
      }
      case M.putRight   => {
        val shape = Shape.deserialize(bytes)
        monoid.putRight(shape)
      }
    }
  }

}

class CompositeMorphism[A,B,C](f:Morphism[B,C], g:Morphism[A,B]) extends Morphism[A,C] {

  val domain = g.domain
  val target = f.target

  def apply(x:A):C = f(g(x))

  override def toString():String = s"($f << $g)"

  def serialize:Iterable[Byte] = Iterable(utilities.Serialization.Morphism.composition) ++ f.serialize ++ g.serialize

}

class TabularMorphism[A,B](val domain:Shape[A],val target:Shape[B],map:container.Container[A,B]) extends Morphism[A,B] {

  def serialize:Iterable[Byte] =
    Iterable(utilities.Serialization.Morphism.table) ++
      domain.serialize ++ target.serialize ++ domain.iterator.flatMap(a => serializeIndex(map(a)))

  def apply(a:A):B = map(a)

}


trait Monomorphism[A,B] extends Morphism[A,B] {

  def o[A0](that:Monomorphism[A0,A]):Monomorphism[A0,B] = new CompositeMonomorphism(this,that)

  val monoInverse:Morphism[B,Either[A,Unit]]

}

class CompositeMonomorphism[A,B,C](f: Monomorphism[B, C], g: Monomorphism[A, B])
  extends CompositeMorphism(f,g) with Monomorphism[A,C] {

  override def toString():String = s"($f << $g)"

  val monoInverse:Morphism[C,Either[A,Unit]] =
    sum.rmap(g.domain,sum.join(I))  <<
    sum.assocRight(g.domain,I,I)    <<
    sum.lmap(g.monoInverse,I)       <<
    f.monoInverse

}


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