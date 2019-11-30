// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.bilinear

import osiris.morphism.product
import osiris.vector.space.VectorSpace
import osiris.vector.{Matrix, Pair, Vector}

class LayeredBilinear[OutI,OutLJ,OutRJ,
                              InI,InLJ,InRJ, S](outer:BilinearFunction[OutI,OutLJ,OutRJ,S],
                                                           inner:BilinearFunction[InI,InLJ,InRJ,S])
  extends BilinearFunction[(OutI,InI),(OutLJ,InLJ),(OutRJ,InRJ),S] {

  val left  = outer.left  * inner.left
  val right = outer.right * inner.right

  val target = outer.target * inner.target

  override def apply(x:Vector[Either[(OutLJ,InLJ),(OutRJ,InRJ)],S]):Vector[(OutI,InI),S] = {
    val xp = x.asPair[(OutLJ,InLJ),(OutRJ,InRJ),Either[(OutLJ,InLJ),(OutRJ,InRJ)]]
    outer.apply[InI,InLJ,InRJ](inner.target,inner.left,inner.right,
      xp.left.asMatrix,xp.right.asMatrix,(l:Vector[InLJ,S],r:Vector[InRJ,S]) => inner(Pair(l,r)))
  }

  def apply[II,JJL,JJR](iiSpace:VectorSpace[II,S],jjlSpace:VectorSpace[JJL,S],jjrSpace:VectorSpace[JJR,S],
                        l:Matrix[(OutLJ,InLJ),JJL,S],r:Matrix[(OutRJ,InRJ),JJR,S],
                        op:(Vector[JJL,S],Vector[JJR,S]) => Vector[II,S]) =
    outer[(InI,II),(InLJ,JJL),(InRJ,JJR)](inner.target*iiSpace,inner.left*jjlSpace,inner.right*jjrSpace,
      l.reIndex(outer.left*(inner.left*jjlSpace),product.assocLeft[OutLJ,InLJ,JJL](outer.left.shape,inner.left.shape,jjlSpace.shape)).asMatrix,
      r.reIndex(outer.right*(inner.right*jjrSpace),product.assocLeft[OutRJ,InRJ,JJR](outer.right.shape,inner.right.shape,jjrSpace.shape)).asMatrix,
      (l:Vector[(InLJ,JJL),S],r:Vector[(InRJ,JJR),S]) =>
        inner[II,JJL,JJR](iiSpace,jjlSpace,jjrSpace,l.asMatrix,r.asMatrix,op)
    ).reIndex((outer.target*inner.target)*iiSpace,product.assocRight[OutI,InI,II](outer.target.shape,inner.target.shape,iiSpace.shape)).asMatrix

  def leftFeedback:BilinearFunction[(OutLJ,InLJ),(OutI,InI),(OutRJ,InRJ),S] =
    outer.leftFeedback & inner.leftFeedback

  def rightFeedback:BilinearFunction[(OutRJ,InRJ),(OutI,InI),(OutLJ,InLJ),S] =
    outer.rightFeedback & inner.rightFeedback

}