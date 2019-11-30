// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.map

import osiris._
import osiris.function.VectorFunction
import osiris.morphism.{product, sum}
import osiris.vector.Vector
import osiris.vector.space.VectorSpace

class ElemWise[I,S](val target:VectorSpace[I,S],op:VectorFunction[Unit,Either[Unit,Unit],S])
  extends VectorFunction[I,Either[I,I],S] {

  val domain = target + target

  override def toString():String = s"ElemWise $target $op"

  private def pair(x: S, y: S): Vector[Either[Unit, Unit], S] = ((I + I) --> scalarSpace) {
    case Left(()) => x
    case Right(()) => y
  }

  def apply(x: Vector[Either[I, I], S]): Vector[I, S] = {
    val xp = x.asPair[I,I,Either[I,I]]
    xp.left.elemWise((x, y) => op(pair(x, y))(()))(xp.right) //feels a bit complicated
  }

  def feedback(x:Vector[Either[I,I],S],y:Vector[I,S]):Vector[Either[I,I],S] = {
    val xm = x.reIndex(target*((I + I) --> scalarSpace),
      sum.bimap(product.getLeft[I](target.shape),product.getLeft[I](target.shape)) o product.leftDistr[I,Unit,Unit](target.shape,I,I)
    ).asMatrix[I,Either[Unit,Unit],(I,Either[Unit,Unit])]
    xm.rowWise((I + I) --> scalarSpace,
      (x:Vector[Either[Unit,Unit],S],y:Vector[Unit,S]) => op.feedback(x,y)
    )(y.toColVector).reIndex(target+target,product.leftExtract(target.shape,I,I) o sum.bimap(product.putLeft[I](target.shape),product.putLeft[I](target.shape)))
  }

}