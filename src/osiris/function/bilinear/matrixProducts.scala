// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.bilinear

import osiris.morphism._
import osiris.vector._
import osiris.vector.space.VectorSpace

case class VectorMatrixProduct[I,J,S](vectorSpace:VectorSpace[J,S],target:VectorSpace[I,S])
  extends BilinearFunction[I,J,(J,I),S] {

  val left = vectorSpace
  val right = vectorSpace * target

  override def toString():String = s"VectorMatrixProduct $vectorSpace $target"

  override def apply(x:Vector[Either[J,(J,I)],S]):Vector[I,S] = {
    val p = x.asPair[J,(J,I),Either[J,(J,I)]]
    p.left * p.right.asMatrix[J,I,(J,I)]
  }

  def apply[I2,JL2,JR2](iSpace:VectorSpace[I2,S],jlSpace:VectorSpace[JL2,S],jrSpace:VectorSpace[JR2,S],
                        left:Matrix[J,JL2,S],right:Matrix[(J,I),JR2,S],
                        op:(Vector[JL2,S],Vector[JR2,S]) => Vector[I2,S]):Matrix[I,I2,S] =
    right.reIndex(target*(vectorSpace*jrSpace),
      product.lmap[(I,J),(J,I),JR2](product.commute[I,J](target.shape,vectorSpace.shape),jrSpace.shape) o product.assocLeft[I,J,JR2](target.shape,vectorSpace.shape,jrSpace.shape)
    ).asMatrix.rowMap(iSpace,(r:Vector[(J,JR2),S]) =>
      r.asMatrix.rowWise[JL2,I2](iSpace,(r:Vector[JR2,S],l:Vector[JL2,S]) => op(l,r))(left).asMatrix.colSum
    )

  def leftFeedback:BilinearFunction[J,I,(J,I),S] =
    VectorMatrixProduct(target,vectorSpace).permuteRight(product.commute(target.shape,vectorSpace.shape))

  def rightFeedback:BilinearFunction[(J,I),I,J,S] =
    OuterProduct(target,left).permuteTarget(product.commute(left.shape,target.shape))

}

case class MatrixVectorProduct[I,J,S](target:VectorSpace[I,S],vectorSpace: VectorSpace[J,S])
  extends BilinearFunction[I,(I,J),J,S] {

  val left = target * vectorSpace
  val right = vectorSpace

  override def toString():String = s"MatrixVectorProduct $target $vectorSpace"

  override def apply(x:Vector[Either[(I,J),J],S]):Vector[I,S] = {
    val p = x.asPair
    p.left.asMatrix * p.right
  }

  def apply[I2,JL2,JR2](iSpace:VectorSpace[I2,S],lSpace:VectorSpace[JL2,S],rSpace:VectorSpace[JR2,S],
                        left:Matrix[(I,J),JL2,S],right:Matrix[J,JR2,S],
                        op:(Vector[JL2,S],Vector[JR2,S]) => Vector[I2,S]):Matrix[I,I2,S] =
    left.reIndex(
      target*(vectorSpace*lSpace),
      product.assocLeft(target.shape,vectorSpace.shape,lSpace.shape)
    ).asMatrix.rowMap(iSpace,(r:Vector[(J,JL2),S]) =>
      (r.asMatrix.rowWise(iSpace,op)(right)).asMatrix.colSum
    )

  def leftFeedback:BilinearFunction[(I,J),I,J,S] = OuterProduct(target,vectorSpace)

  def rightFeedback:BilinearFunction[J,I,(I,J),S] = VectorMatrixProduct(target,vectorSpace)

}

case class MatrixMatrixProduct[I,K,J,S](outer:VectorSpace[I,S],
                                                   middle:VectorSpace[K,S],
                                                   inner:VectorSpace[J,S])
  extends BilinearFunction[(I,J),(I,K),(K,J),S] {

  val  left = outer * middle
  val right = middle * inner

  val target = outer * inner

  override def toString():String = s"MatrixMatrixProduct $outer $middle $inner"

  override def apply(x:Vector[Either[(I,K),(K,J)],S]):Vector[(I,J),S] = {
    val p = x.asPair[(I,K),(K,J),Either[(I,K),(K,J)]]
    p.left.asMatrix[I,K,(I,K)] * p.right.asMatrix[K,J,(K,J)]
  }

  def apply[I2,JL2,JR2](iSpace:VectorSpace[I2,S],jlSpace:VectorSpace[JL2,S],jrSpace:VectorSpace[JR2,S],
                        left:Matrix[(I,K),JL2,S],right:Matrix[(K,J),JR2,S],
                        op:(Vector[JL2,S],Vector[JR2,S]) => Vector[I2,S]):Matrix[(I,J),I2,S] = {

    val lft = left.reIndex(outer*(middle*jlSpace),product.assocLeft[I,K,JL2](outer.shape,middle.shape,jlSpace.shape)).asMatrix[I,(K,JL2),(I,(K,JL2))]

    val rgt = right.reIndex(inner*(middle*jrSpace),
      product.lmap[(J,K),(K,J),JR2](product.commute[J,K](inner.shape,middle.shape),jrSpace.shape) o product.assocLeft(inner.shape,middle.shape,jrSpace.shape)
    ).asMatrix[J,(K,JR2),(J,(K,JR2))]

    lft.rowMap(inner*iSpace,(row:Vector[(K,JL2),S]) =>
      rgt.rowMap(iSpace, (col:Vector[(K,JR2),S]) =>
        row.asMatrix[K,JL2,(K,JL2)].rowWise(iSpace,op)(col.asMatrix[K,JR2,(K,JR2)]).colSum
      )
    ).reIndex((outer*inner)*iSpace,product.assocRight[I,J,I2](outer.shape,inner.shape,iSpace.shape)).asMatrix
  }

  def leftFeedback:BilinearFunction[(I,K),(I,J),(K,J),S] =
    MatrixMatrixProduct(outer,inner,middle).permuteRight(product.commute(inner.shape,middle.shape))

  def rightFeedback:BilinearFunction[(K,J),(I,J),(I,K),S] =
    MatrixMatrixProduct(inner,outer,middle).permuteLeft(
      product.commute(inner.shape,outer.shape)
    ).permuteTarget(product.commute(middle.shape,inner.shape))

}
