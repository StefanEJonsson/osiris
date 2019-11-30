// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.bilinear

import osiris.morphism.product
import osiris.vector.{Matrix, Pair, Vector}
import osiris.vector.space.VectorSpace

class RowWiseBilinear[I,J,L,R,S](outer:VectorSpace[I,S], f:BilinearFunction[J,L,R,S])
  extends BilinearFunction[(I,J),(I,L),(I,R),S] {

  val left = outer * f.left
  val right = outer * f.right

  val target = outer * f.target

  override def apply(x:Vector[Either[(I,L),(I,R)],S]):Matrix[I,J,S] = {
    val xp = x.asPair
    xp.left.asMatrix.rowWise(f.target,(x:Vector[L,S],y:Vector[R,S]) => f(Pair(x,y)))(xp.right.asMatrix)
  }

  def apply[II,LL,RR](iiSpace:VectorSpace[II,S],llSpace:VectorSpace[LL,S],rrSpace:VectorSpace[RR,S],
                      l:Matrix[(I,L),LL,S],r:Matrix[(I,R),RR,S],
                      op:(Vector[LL,S],Vector[RR,S]) => Vector[II,S]):Matrix[(I,J),II,S] = {

    val ll = l.reIndex(outer*(f.left*llSpace),product.assocLeft[I,L,LL](outer.shape,f.left.shape,llSpace.shape)).asMatrix[I,(L,LL),(I,(L,LL))]
    val rr = r.reIndex(outer*(f.right*rrSpace),product.assocLeft[I,R,RR](outer.shape,f.right.shape,rrSpace.shape)).asMatrix[I,(R,RR),(I,(R,RR))]

    ll.rowWise(f.target*iiSpace, (x:Vector[(L,LL),S],y:Vector[(R,RR),S]) =>
      f[II,LL,RR](iiSpace,llSpace,rrSpace,x.asMatrix,y.asMatrix,op)
    )(rr).reIndex(target*iiSpace,
      product.assocRight[I,J,II](outer.shape,f.target.shape,iiSpace.shape)
    ).asMatrix
  }


  def leftFeedback = new RowWiseBilinear(outer,f.leftFeedback)

  def rightFeedback = new RowWiseBilinear(outer,f.rightFeedback)

}