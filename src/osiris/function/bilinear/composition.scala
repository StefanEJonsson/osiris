// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.bilinear

import osiris.function.linear.LinearFunction
import osiris.utilities.serialization.v2
import osiris.vector._
import osiris.vector.space.VectorSpace

/**
  * The composition of a linear function with a bilinear function. The resulting function is also bilinear.
  * @param f the linear function
  * @param g the bilinear function
  * @tparam I The index type of the vectors returned from f
  * @tparam J The index type of the vectors returned from g and used as input for f
  * @tparam L The index type of the vectors used as the first argument to g
  * @tparam R The index type of the vectors used as the second argument to g
  * @tparam S The scalar field
  */
class LinearComposeBilinear[I,J,L,R,S](f:LinearFunction[I,J,S],g:BilinearFunction[J,L,R,S])
  extends BilinearFunction[I,L,R,S] {

  val left = g.left
  val right = g.right

  val target = f.target

  override def toString():String = s"($f << $g)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.linearComposeBilinear) ++
    f.serialize ++ g.serialize

  def rightFeedback:BilinearFunction[R,I,L,S] = g.rightFeedback.<</(f.linearFeedback)

  def leftFeedback:BilinearFunction[L,I,R,S] = g.leftFeedback.<</(f.linearFeedback)

  override def apply(x:Vector[Either[L,R],S]):Vector[I,S] = f(g(x))

  def apply[I2,L2,R2](iiSpace:VectorSpace[I2,S],llSpace:VectorSpace[L2,S],rrSpace:VectorSpace[R2,S],
                      l:Matrix[L,L2,S],r:Matrix[R,R2,S],
                      op:(Vector[L2,S],Vector[R2,S])=>Vector[I2,S]):Matrix[I,I2,S] =
    g(iiSpace,llSpace,rrSpace,l,r,op).colMap(f.target,f)

}

/**
  * The composition of a bilinear function with a linear function "from the left". The resulting function is a new
  * bilinear function that does the same thing as the original bilinear function but first applies the linear function
  * to its left argument.
  *
  * @param f the original bilinear function
  * @param g the linear function applied to the left argument
  * @tparam I The index type of the vectors returned from the bilinear function
  * @tparam L2 The index type of the output from g and the left input to f
  * @tparam L1 The index type of the input to g
  * @tparam R The index type of the second argument of f
  * @tparam S The scalar field
  */
class BilinearLeftComposeLinear[I,L2,L1,R,S](f:BilinearFunction[I,L2,R,S],g:LinearFunction[L2,L1,S])
  extends BilinearFunction[I,L1,R,S] {

  val left = g.domain
  val right = f.right
  val target = f.target

  override def toString():String = s"($f <</ $g)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.bilinearLeftComposeLinear) ++
    f.serialize ++ g.serialize

  override def apply(x:Vector[Either[L1,R],S]):Vector[I,S] = f(x.asPair.lmap(g))

  def apply[II,LL,RR](iiSpace:VectorSpace[II,S],llSpace:VectorSpace[LL,S],rrSpace:VectorSpace[RR,S],
                      l:Matrix[L1,LL,S],r:Matrix[R,RR,S],
                      op:(Vector[LL,S],Vector[RR,S])=>Vector[II,S]):Matrix[I,II,S] = {

    val ll = l.colMap(g.target,(x:Vector[L1,S]) => g(x)).asMatrix[L2,LL,(L2,LL)]
    f(iiSpace,llSpace,rrSpace,ll,r,op)
  }

  def  leftFeedback = g.linearFeedback << f.leftFeedback
  def rightFeedback = f.rightFeedback <<\ g

}

/**
  * The composition of a bilinear function with a linear function "from the right". The resulting function is a new
  * bilinear function that does the same thing as the original bilinear function but first applies the linear function
  * to its right argument.
  *
  * @param f the original bilinear function
  * @param g the linear function applied to the right argument
  * @tparam I The index type of the vectors returned from the bilinear function
  * @tparam R2 The index type of the output from g and the right input to f
  * @tparam R1 The index type of the input to g
  * @tparam L The index type of the left argument of f
  * @tparam S The scalar field
  */
class BilinearRightComposeLinear[I,L,R1,R2,S](f:BilinearFunction[I,L,R2,S],g:LinearFunction[R2,R1,S])
  extends BilinearFunction[I,L,R1,S] {

  val left = f.left
  val right = g.domain
  val target = f.target

  override def toString():String = s"($f <<\\ $g)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.bilinearRightComposeLinear) ++
    f.serialize ++ g.serialize

  override def apply(x:Vector[Either[L,R1],S]):Vector[I,S] = f(x.asPair.rmap(g))

  def apply[II,LL,RR](iiSpace:VectorSpace[II,S],llSpace:VectorSpace[LL,S],rrSpace:VectorSpace[RR,S],
                      l:Matrix[L,LL,S],r:Matrix[R1,RR,S],
                      op:(Vector[LL,S],Vector[RR,S])=>Vector[II,S]):Matrix[I,II,S] = {

    val rr = r.colMap(g.target,(x:Vector[R1,S]) => g(x)).asMatrix[R2,RR,(R2,RR)]
    f(iiSpace,llSpace,rrSpace,l,rr,op)
  }

  def  leftFeedback = f.leftFeedback <<\ g
  def rightFeedback = g.linearFeedback << f.rightFeedback

}