// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear

import osiris._
import vector.Vector
import osiris.vector.space.SequentialSpace
import function.bilinear.{ComplexMultiplication, MatrixVectorProduct}
import osiris.utilities.serialization.v2

/**
  * Computes the frequency content of a vector.
  *
  * DFT takes a sequential vector of complex numbers and produces a sequential vector of complex numbers. Note that
  * since complex numbers in osiris are represented as vectors, vectors of complex numbers are technically matrices.
  */
class DiscreteFourierTransform[S](n:SequentialSpace[S])
  extends LinearFunction[(Int,Either[Unit,Unit]),(Int,Either[Unit,Unit]),S] {

  if (n.shape.start != 0) {
    throw new IllegalArgumentException(s"Use zero indexed vectors!")
  }

  override val scalarSpace = n.scalarSpace
  private val C = (I + I) --> n.scalarSpace

  override def toString():String = s"DFT[$n]"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.dft) ++ n.shape.serialize

  //TODO make sure that FFT algorithm gets used instead if n is a power of 2

  private val powers =
    scala.collection.immutable.Vector.tabulate(n.shape.size)(i => C((e:Either[Unit,Unit]) => e match {
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

  private val swap = new reindex.Permute(scalarSpace,product.rmap(n.shape,sum.commute(I,I)))

  def linearFeedback:LinearFunction[(Int,Either[Unit,Unit]),(Int,Either[Unit,Unit]),S] =
    swap << this << swap

}
