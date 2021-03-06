package osiris.utilities.serialization.v2

import osiris.shape.{Range, Shape}
import osiris.{*, +, utilities}

object Morphism {

  object constants {
    val id:Byte = 1
    val unit:Byte = 2
    val absurd:Byte = 3
    val constant:Byte = 4
    val table:Byte = 5

    val composition:Byte = 10

    val lmap:Byte = 15
    val rmap:Byte = 16
    val bimap:Byte = 17

    val sum:Byte = 20
    val product:Byte = 21

    val equal:Byte = 30

    val and:Byte = 35
    val or:Byte = 36
    val xor:Byte = 37

    val translate:Byte = 40
    val inv:Byte = 41

    val commute:Byte = 50

    val leftAssoc:Byte = 51
    val rightAssoc:Byte = 52

    val putLeft:Byte = 53
    val putRight:Byte = 54
    val getLeft:Byte = 55
    val getRight:Byte = 56

    val distrLeft:Byte = 57
    val distrRight:Byte = 58
    val extractLeft:Byte = 59
    val extractRight:Byte = 60

    val join:Byte = 70
    val left:Byte = 71
    val right:Byte = 72

    val spookLeft:Byte = 80
    val spookRight:Byte = 81
    val bustLeft:Byte = 82
    val bustRight:Byte = 83

    val first:Byte = 84
    val second:Byte = 85

    val firstWith:Byte = 86
    val secondWith:Byte = 87

    val copy:Byte = 88
  }

  import Morphism.{constants => M}

  import osiris.morphism._

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
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        id[shape.Type](shape.asInstanceOf[Shape[shape.Type]])
      }
      case M.unit => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        unit(shape)
      }
      case M.absurd => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        absurd(shape)
      }
      case M.constant => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        val idx:shape.Type = shape.deserializeIndex(bytes).asInstanceOf[shape.Type]
        constant(shape.asInstanceOf[Shape[shape.Type]],idx)
      }
      case M.table => {
        val domain = utilities.serialization.v2.Shape.deserialize(bytes)
        val target = utilities.serialization.v2.Shape.deserialize(bytes)
        type D = domain.Type
        type T = target.Type
        val d = domain.asInstanceOf[Shape[D]]
        val t = target.asInstanceOf[Shape[T]]
        val map = (d -->[T]())((_:D) => t.deserializeIndex(bytes))
        new TabularMorphism[D,T](d,t,map)
      }
      case M.translate => {
        val range = utilities.serialization.v2.Shape.deserialize(bytes).asInstanceOf[Range]
        val dx = Primitives.deserializeInt(bytes)
        int.translate(dx,range)
      }
      case M.inv => {
        val range = utilities.serialization.v2.Shape.deserialize(bytes).asInstanceOf[Range]
        int.inv(range)
      }

      case M.equal => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        bool.equal(shape)
      }
      case M.and => bool.and
      case M.or => bool.or
      case M.xor => bool.xor

      case M.left => {
        val l = utilities.serialization.v2.Shape.deserialize(bytes)
        val r = utilities.serialization.v2.Shape.deserialize(bytes)
        sum.left(l,r)
      }
      case M.right => {
        val l = utilities.serialization.v2.Shape.deserialize(bytes)
        val r = utilities.serialization.v2.Shape.deserialize(bytes)
        sum.right(l,r)
      }
      case M.join => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        sum.join(shape)
      }

      case M.distrLeft => {
        val a = utilities.serialization.v2.Shape.deserialize(bytes)
        val b = utilities.serialization.v2.Shape.deserialize(bytes)
        val c = utilities.serialization.v2.Shape.deserialize(bytes)
        product.leftDistr(a,b,c)
      }
      case M.distrRight => {
        val a = utilities.serialization.v2.Shape.deserialize(bytes)
        val b = utilities.serialization.v2.Shape.deserialize(bytes)
        val c = utilities.serialization.v2.Shape.deserialize(bytes)
        product.rightDistr(a,b,c)
      }
      case M.extractLeft => {
        val a = utilities.serialization.v2.Shape.deserialize(bytes)
        val b = utilities.serialization.v2.Shape.deserialize(bytes)
        val c = utilities.serialization.v2.Shape.deserialize(bytes)
        product.leftExtract(a,b,c)
      }
      case M.extractRight => {
        val a = utilities.serialization.v2.Shape.deserialize(bytes)
        val b = utilities.serialization.v2.Shape.deserialize(bytes)
        val c = utilities.serialization.v2.Shape.deserialize(bytes)
        product.rightExtract(a,b,c)
      }

      case M.first => {
        val fst = utilities.serialization.v2.Shape.deserialize(bytes)
        val snd = utilities.serialization.v2.Shape.deserialize(bytes)
        product.first(fst,snd)
      }
      case M.second => {
        val fst = utilities.serialization.v2.Shape.deserialize(bytes)
        val snd = utilities.serialization.v2.Shape.deserialize(bytes)
        product.second(fst,snd)
      }
      case M.firstWith => {
        val fst = utilities.serialization.v2.Shape.deserialize(bytes)
        val snd = utilities.serialization.v2.Shape.deserialize(bytes)
        val c:snd.Type = snd.deserializeIndex(bytes).asInstanceOf[snd.Type]
        product.leftPairedWith(fst.asInstanceOf[Shape[fst.Type]],snd.asInstanceOf[Shape[snd.Type]],c)
      }
      case M.secondWith => {
        val fst = utilities.serialization.v2.Shape.deserialize(bytes)
        val snd = utilities.serialization.v2.Shape.deserialize(bytes)
        val c:fst.Type = fst.deserializeIndex(bytes).asInstanceOf[fst.Type]
        product.rightPairedWith(fst.asInstanceOf[Shape[fst.Type]],snd.asInstanceOf[Shape[snd.Type]],c)
      }

      case M.spookLeft => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        product.spookLeft(shape)
      }
      case M.spookRight => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        product.spookRight(shape)
      }
      case M.bustLeft => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        product.bustLeft(shape)
      }
      case M.bustRight => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        product.bustRight(shape)
      }

      case M.copy => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
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
        (a,b) match {
          case (a:Isomorphism[_,_],b:Isomorphism[_,_]) => monoid.bimap(a,b)
          case (a:Monomorphism[_,_],b:Monomorphism[_,_]) => monoid.bimap(a,b)
          case (a:Morphism[_,_],b:Morphism[_,_]) => monoid.bimap(a,b)
        }
      }
      case M.lmap => {
        val a = deserialize(bytes)
        val r = utilities.serialization.v2.Shape.deserialize(bytes)
        a match {
          case (a:Isomorphism[_,_]) => monoid.lmap(a,r)
          case (a:Monomorphism[_,_]) => monoid.lmap(a,r)
          case (a:Morphism[_,_]) => monoid.lmap(a,r)
        }
      }
      case M.rmap => {
        val l = utilities.serialization.v2.Shape.deserialize(bytes)
        val b = deserialize(bytes)
        b match {
          case (b:Isomorphism[_,_]) => monoid.rmap(l,b)
          case (b:Monomorphism[_,_]) => monoid.rmap(l,b)
          case (b:Morphism[_,_]) => monoid.rmap(l,b)
        }
      }
      case M.commute    => {
        val a = utilities.serialization.v2.Shape.deserialize(bytes)
        val b = utilities.serialization.v2.Shape.deserialize(bytes)
        monoid.commute(a,b)
      }
      case M.leftAssoc  => {
        val a = utilities.serialization.v2.Shape.deserialize(bytes)
        val b = utilities.serialization.v2.Shape.deserialize(bytes)
        val c = utilities.serialization.v2.Shape.deserialize(bytes)
        monoid.assocLeft(a,b,c)
      }
      case M.rightAssoc => {
        val a = utilities.serialization.v2.Shape.deserialize(bytes)
        val b = utilities.serialization.v2.Shape.deserialize(bytes)
        val c = utilities.serialization.v2.Shape.deserialize(bytes)
        monoid.assocRight(a,b,c)
      }
      case M.getLeft    => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        monoid.getLeft(shape)
      }
      case M.getRight   => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        monoid.getRight(shape)
      }
      case M.putLeft    => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        monoid.putLeft(shape)
      }
      case M.putRight   => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        monoid.putRight(shape)
      }
    }
  }

}