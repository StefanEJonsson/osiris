// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.morphism

import osiris._
import osiris.morphism.properties.{AbelianProperty, Bifunctor, Distributivity, Monoidicity}
import osiris.shape.Shape

object product extends Bifunctor[*] with Monoidicity[*,Unit] with AbelianProperty[*]
  with Distributivity[*,+] {

  protected val i = I

  protected def F[A,B](a:Shape[A], b:Shape[B]):Shape[*[A,B]] = a * b
  protected def G[A,B](a:Shape[A], b:Shape[B]):Shape[+[A,B]] = a + b

  protected def code = Iterable(utilities.Serialization.Morphism.product)

  override def toString():String = "product"

  //Bifunctoriality

  def bim[L1,L2,R1,R2](l:Morphism[L1,L2],r:Morphism[R1,R2]): Morphism[*[L1,R1],*[L2, R2]] =
    new Morphism[*[L1,R1],*[L2,R2]] {

      val domain = l.domain * r.domain
      val target = l.target * r.target

      override def toString():String = s"${product.this}.bimap($l,$r)"

      def serialize:Iterable[Byte] =
        Iterable(utilities.Serialization.Morphism.bimap) ++
        l.serialize ++ r.serialize

      def apply(x: *[L1,R1]): *[L2,R2] = (l(x._1),r(x._2))
    }

  //Monoidicity

  protected def gl[A](x: *[A,Unit]) = x._1

  protected def gr[A](x: *[Unit,A]) = x._2

  protected def pl[A](x:A): *[A,Unit] = (x,())

  protected def pr[A](x:A): *[Unit,A] = ((),x)

  protected def al[A,B,C](x: *[A,*[B,C]]): *[*[A,B],C] = ((x._1,x._2._1),x._2._2)

  protected def ar[A,B,C](x: *[*[A,B],C]): *[A,*[B,C]] = (x._1._1,(x._1._2,x._2))

  protected def commutation[A,B](x: *[A,B]): *[B,A] = x.swap

  //Distributivity

  protected def ld[A,B,C](x: *[A,+[B,C]]): +[*[A,B],*[A,C]] = x._2 match {
    case  Left(b) =>  Left((x._1,b))
    case Right(c) => Right((x._1,c))
  }

  protected def rd[A,B,C](x:(Either[A,B],C)):Either[(A,C),(B,C)] = x._1 match {
    case  Left(a) =>  Left((a,x._2))
    case Right(b) => Right((b,x._2))
  }

  protected def le[A,B,C](x:(Either[(A,B),(A,C)])):(A,Either[B,C]) = x match {
    case  Left((a,b)) => (a, Left(b))
    case Right((a,c)) => (a,Right(c))
  }

  protected def re[A,B,C](x:Either[(A,C),(B,C)]):(Either[A,B],C) = x match {
    case  Left((a,c)) => ( Left(a),c)
    case Right((b,c)) => (Right(b),c)
  }

  //multiplication by zero

  def bustLeft[A](shape:Shape[A]) = new Isomorphism[(A,Nothing),Nothing] {

    val domain = shape *[Nothing] O
    val target = O

    lazy val inverse = spookLeft(shape)

    override def toString():String = s"bustLeft($shape)"

    def apply(x:(A,Nothing)):Nothing = x._2

    def serialize: Iterable[Byte] = Iterable(utilities.Serialization.Morphism.bustLeft) ++ shape.serialize

  }

  def bustRight[A](shape:Shape[A]) = new Isomorphism[(Nothing,A),Nothing] {

    val domain = O * shape
    val target = O

    lazy val inverse = spookRight(shape)

    override def toString():String = s"bustRight($shape)"

    def apply(x:(Nothing,A)):Nothing = x._1

    def serialize: Iterable[Byte] = Iterable(utilities.Serialization.Morphism.bustRight) ++ shape.serialize

  }

  def spookLeft[A](shape:Shape[A]):Isomorphism[Nothing,(A,Nothing)] = new Isomorphism[Nothing,(A,Nothing)] {

    val domain = O
    val target = shape *[Nothing] O

    lazy val inverse = bustLeft(shape)

    override def toString():String = s"spookLeft($shape)"

    def apply(x:Nothing):(A,Nothing) = absurd(target)(x)

    def serialize: Iterable[Byte] = Iterable(utilities.Serialization.Morphism.spookLeft) ++ shape.serialize

  }

  def spookRight[A](shape:Shape[A]):Isomorphism[Nothing,(Nothing,A)] = new Isomorphism[Nothing,(Nothing,A)] {

    val domain = O
    val target = O * shape

    lazy val inverse = bustRight(shape)

    override def toString():String = s"spookRight($shape)"

    def apply(x:Nothing):(Nothing,A) = absurd(target)(x)

    def serialize: Iterable[Byte] = Iterable(utilities.Serialization.Morphism.spookRight) ++ shape.serialize

  }

  //epimorphisms

  def first[A,B](l:Shape[A], r:Shape[B]) = new Morphism[(A,B),A] {

    val domain = l*r
    val target = l

    override def toString():String = s"product.first($l,$r)"

    def apply(x:(A,B)):A = x._1

    def serialize: Iterable[Byte] = Iterable(utilities.Serialization.Morphism.first) ++ l.serialize ++ r.serialize

  }

  def second[A,B](l:Shape[A], r:Shape[B]) = new Morphism[(A,B),B] {

    val domain = l*r
    val target = r

    override def toString():String = s"product.second($l,$r)"

    def apply(x:(A,B)):B = x._2

    def serialize: Iterable[Byte] = Iterable(utilities.Serialization.Morphism.second) ++ l.serialize ++ r.serialize

  }

  //monomorphisms

  def leftPairedWith[A,B](l:Shape[A],r:Shape[B],y:B) = new Monomorphism[A,(A,B)] {

    val domain = l
    val target = l*r

    lazy val monoInverse:Morphism[(A,B),Either[A,Unit]] =
      sum.commute(I,l) <<
      sum.bimap(second(l,I),getLeft(l)) <<
      leftDistr(l,I,I) <<
      rmap(l,bool.equals(r,y))

    override def toString():String = s"leftPairedWith($l,$r,$y)"

    def apply(x:A):(A,B) = (x,y)

    def serialize: Iterable[Byte] =
      Iterable(utilities.Serialization.Morphism.firstWith) ++ l.serialize ++ r.serialize ++ Shape.serializeIndex(y)

  }

  def rightPairedWith[A,B](l:Shape[A],r:Shape[B],x:A) = new Monomorphism[B,(A,B)] {

    val domain = r
    val target = l*r

    lazy val monoInverse:Morphism[(A,B),Either[B,Unit]] =
      sum.commute(I,r) <<
      sum.bimap(first(I,r),getRight(r)) <<
      rightDistr(I,I,r) <<
      lmap(bool.equals(l,x),r)

    override def toString():String = s"rightPairedWith($l,$r,$x)"

    def apply(y:B):(A,B) = (x,y)

    def serialize: Iterable[Byte] =
      Iterable(utilities.Serialization.Morphism.secondWith) ++ l.serialize ++ r.serialize ++ Shape.serializeIndex(x)


  }

  def copy[A](shape:Shape[A]):Monomorphism[A,(A,A)] = new Monomorphism[A,(A,A)] {

    val domain = shape
    val target = shape*shape

    lazy val monoInverse:Morphism[(A,A),Either[A,Unit]] =
      sum.commute(I,shape) << sum.bimap(second(shape,I),getLeft(shape)) <<
      leftDistr(shape,I,I) <<
      rmap(shape,bool.equal(shape)) <<
      assocRight(shape,shape,shape) <<
      lmap(copy(shape),shape)

    override def toString():String = s"copy($shape)"

    def apply(x:A):(A,A) = (x,x)

    def serialize: Iterable[Byte] =
      Iterable(utilities.Serialization.Morphism.copy) ++ shape.serialize

  }

  def lMapMonoInverse[L1,L2,R](f:Monomorphism[L1,L2], r:Shape[R]):Morphism[*[L2,R],Either[*[L1,R],Unit]] =
    sum.rmap(f.domain*r,unit(I*r)) <<
    rightDistr(f.domain,I,r) <<
    lmap(f.monoInverse,r)

  def rMapMonoInverse[L,R1,R2](l:Shape[L],f:Monomorphism[R1,R2]):Morphism[*[L,R2],+[*[L,R1],Unit]] =
    sum.rmap(l*f.domain,unit(l*I))  <<
    leftDistr(l,f.domain,I)         <<
    rmap(l,f.monoInverse)

  def biMapMonoInverse[L1,L2,R1,R2](l:Monomorphism[L1,L2], r:Monomorphism[R1,R2]):Morphism[*[L2,R2],+[*[L1,R1],Unit]] =
    sum.rmap(l.domain*r.domain,unit(I + I)) <<
    sum.assocRight(l.domain*r.domain,I,I) <<
    sum.bimap(sum.rmap(l.domain*r.domain,unit(l.domain*I)) << leftDistr(l.domain,r.domain,I),unit(I*(r.domain + I))) <<
    rightDistr(l.domain,I,r.domain + I) << bimap(l.monoInverse,r.monoInverse)

}
