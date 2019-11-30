// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.bilinear

import osiris._
import morphism.product
import vector.{Matrix, Pair, Vector}
import vector.space.VectorSpace

class ColWiseBilinear[I,J,L,R,S](inner:VectorSpace[J,S], f:BilinearFunction[I,L,R,S])
  extends BilinearFunction[(I,J),(L,J),(R,J),S] {

  val left = f.left * inner
  val right = f.right * inner

  val target = f.target * inner

  override def apply(x:Vector[Either[(L,J),(R,J)],S]):Matrix[I,J,S] = {
    val xp = x.asPair
    xp.left.asMatrix.colWise(f.target,(x:Vector[L,S],y:Vector[R,S]) => f(Pair(x,y)))(xp.right.asMatrix)
  }

  def apply[II,LL,RR](iiSpace:VectorSpace[II,S],llSpace:VectorSpace[LL,S],rrSpace:VectorSpace[RR,S],
                      l:Matrix[(L,J),LL,S],r:Matrix[(R,J),RR,S],
                      op:(Vector[LL,S],Vector[RR,S]) => Vector[II,S]):Matrix[(I,J),II,S] = {

    val ll = l.reIndex(
        product.assocLeft(f.left.shape,inner.shape,llSpace.shape) o
        product.rmap(f.left.shape,product.commute[LL,J](llSpace.shape,inner.shape)) o
        product.assocRight[L,LL,J](f.left.shape,llSpace.shape,inner.shape)
    ).asMatrix[(L,LL),J,((L,LL),J)]

    val rr = r.reIndex(
      product.assocLeft(f.right.shape,inner.shape,rrSpace.shape) o
        product.rmap[R,*[RR,J],*[J,RR]](f.right.shape,product.commute[RR,J](rrSpace.shape,inner.shape)) o
        product.assocRight[R,RR,J](f.right.shape,rrSpace.shape,inner.shape)
    ).asMatrix[(R,RR),J,((R,RR),J)]

    ll.colWise(f.target*iiSpace,(x:Vector[(L,LL),S],y:Vector[(R,RR),S]) =>
      f(iiSpace,llSpace,rrSpace,x.asMatrix[L,LL,(L,LL)],y.asMatrix[R,RR,(R,RR)],op)
    )(rr).reIndex((f.target*inner)*iiSpace,
      product.assocLeft(f.target.shape,iiSpace.shape,inner.shape) o
        product.rmap[I,(J,II),(II,J)](f.target.shape,product.commute(inner.shape,iiSpace.shape)) o
        product.assocRight[I,J,II](f.target.shape,inner.shape,iiSpace.shape)
    ).asMatrix
  }

  def leftFeedback = new ColWiseBilinear(inner,f.leftFeedback)
  def rightFeedback = new ColWiseBilinear(inner,f.rightFeedback)
}
