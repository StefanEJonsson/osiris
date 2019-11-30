// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.bilinear

import osiris.morphism.product
import osiris.vector.space.VectorSpace
import osiris.vector.{Matrix, Vector}

case class OuterProduct[I,J,S](left:VectorSpace[I,S],right:VectorSpace[J,S])
  extends BilinearFunction[(I,J),I,J,S] {

  val target = left * right

  override def toString():String = s"OuterProduct $left $right"

  override def apply(x:Vector[Either[I,J],S]):Vector[(I,J),S] = {
    val p = x.asPair[I,J,Either[I,J]]
    p.left >< p.right
  }

  def apply[I2,JL2,JR2](iSpace:VectorSpace[I2,S],lSpace:VectorSpace[JL2,S],rSpace:VectorSpace[JR2,S],
                        left:Matrix[I,JL2,S],right:Matrix[J,JR2,S],
                        op:(Vector[JL2,S],Vector[JR2,S]) => Vector[I2,S]):Matrix[(I,J),I2,S] =
    left.rowMap (this.right*iSpace, (r:Vector[JL2,S]) =>
        right.rowMap(iSpace,(c:Vector[JR2,S]) => op(r,c))
    ).reIndex((this.left*this.right)*iSpace, product.assocRight[I,J,I2](this.left.shape,this.right.shape,iSpace.shape)).asMatrix

  def leftFeedback:BilinearFunction[I,(I,J),J,S] = MatrixVectorProduct(left,right)

  def rightFeedback:BilinearFunction[J,(I,J),I,S] =
    MatrixVectorProduct(right,left).permuteLeft(product.commute(right.shape,left.shape))

}