// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.bilinear

import osiris.function.linear.LinearFunction
import osiris.vector._
import osiris.vector.space.VectorSpace

class LinearComposeBilinear[I,J,L,R,S](f:LinearFunction[I,J,S],g:BilinearFunction[J,L,R,S])
  extends BilinearFunction[I,L,R,S] {

  val left = g.left
  val right = g.right

  val target = f.target

  def rightFeedback:BilinearFunction[R,I,L,S] = g.rightFeedback.<</(f.feedback)

  def leftFeedback:BilinearFunction[L,I,R,S] = g.leftFeedback.<</(f.feedback)

  override def apply(x:Vector[Either[L,R],S]):Vector[I,S] = f(g(x))

  def apply[I2,L2,R2](iiSpace:VectorSpace[I2,S],llSpace:VectorSpace[L2,S],rrSpace:VectorSpace[R2,S],
                      l:Matrix[L,L2,S],r:Matrix[R,R2,S],
                      op:(Vector[L2,S],Vector[R2,S])=>Vector[I2,S]):Matrix[I,I2,S] =
    g(iiSpace,llSpace,rrSpace,l,r,op).colMap(f.target,f)

}

class BilinearLeftComposeLinear[I,L2,L1,R,S](f:BilinearFunction[I,L2,R,S],g:LinearFunction[L2,L1,S])
  extends BilinearFunction[I,L1,R,S] {

  val left = g.domain
  val right = f.right
  val target = f.target

  override def apply(x:Vector[Either[L1,R],S]):Vector[I,S] = f(x.asPair.lmap(g))

  def apply[II,LL,RR](iiSpace:VectorSpace[II,S],llSpace:VectorSpace[LL,S],rrSpace:VectorSpace[RR,S],
                      l:Matrix[L1,LL,S],r:Matrix[R,RR,S],
                      op:(Vector[LL,S],Vector[RR,S])=>Vector[II,S]):Matrix[I,II,S] = {

    val ll = l.colMap(g.target,(x:Vector[L1,S]) => g(x)).asMatrix[L2,LL,(L2,LL)]
    f(iiSpace,llSpace,rrSpace,ll,r,op)
  }

  def  leftFeedback = g.feedback << f.leftFeedback
  def rightFeedback = f.rightFeedback <<\ g

}

class BilinearRightComposeLinear[I,L,R1,R2,S](f:BilinearFunction[I,L,R2,S],g:LinearFunction[R2,R1,S])
  extends BilinearFunction[I,L,R1,S] {

  val left = f.left
  val right = g.domain
  val target = f.target

  override def apply(x:Vector[Either[L,R1],S]):Vector[I,S] = f(x.asPair.rmap(g))

  def apply[II,LL,RR](iiSpace:VectorSpace[II,S],llSpace:VectorSpace[LL,S],rrSpace:VectorSpace[RR,S],
                      l:Matrix[L,LL,S],r:Matrix[R1,RR,S],
                      op:(Vector[LL,S],Vector[RR,S])=>Vector[II,S]):Matrix[I,II,S] = {

    val rr = r.colMap(g.target,(x:Vector[R1,S]) => g(x)).asMatrix[R2,RR,(R2,RR)]
    f(iiSpace,llSpace,rrSpace,l,rr,op)
  }

  def  leftFeedback = f.leftFeedback <<\ g
  def rightFeedback = g.feedback << f.rightFeedback

}