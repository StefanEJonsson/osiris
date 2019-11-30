// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear

import osiris._
import vector.Vector
import osiris.vector.space.SequentialSpace
import function.bilinear.{MatrixVectorProduct,ComplexMultiplication}

class DiscreteFourierTransform[S](n:SequentialSpace[S])
  extends LinearFunction[(Int,Either[Unit,Unit]),(Int,Either[Unit,Unit]),S] {

  if (n.shape.start != 0) {
    throw new IllegalArgumentException(s"Use zero indexed vectors!")
  }

  override val scalarSpace = n.scalarSpace
  private val C = (I + I) --> n.scalarSpace

  //TODO make sure that FFT algorithm gets used instead if n is a power of 2

  private val powers =
    Vector.tabulate(n.shape.size)(i => C((e:Either[Unit,Unit]) => e match {
      case Left(()) => scalarSpace.cos(scalarSpace.fromDouble(-2*i*Math.PI))
      case Right(()) => scalarSpace.sin(scalarSpace.fromDouble(-2*i*Math.PI))
    }))

  private val fourierMatrix = ((n * n) * C).rows( (t:(Int,Int)) =>
    powers(t._1*t._2 % n.shape.size)*scalarSpace.fromDouble(1/Math.sqrt(n.shape.size))
  )

  private val applicationFunction = MatrixVectorProduct(n,n) & new ComplexMultiplication(scalarSpace)

  val domain = n * C
  val target = n * C

  def apply(x:Vector[(Int,Either[Unit,Unit]),S]):Vector[(Int,Either[Unit,Unit]),S] =
    applicationFunction(fourierMatrix|x)

  import morphism._

  private val swap = new reindex.Permute(target,target,product.rmap(n.shape,sum.commute(I,I)))

  def feedback:LinearFunction[(Int,Either[Unit,Unit]),(Int,Either[Unit,Unit]),S] =
    swap << this << swap

}
