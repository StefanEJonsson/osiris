// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.bilinear

import osiris._
import osiris.morphism._
import osiris.utilities.serialization.v2
import osiris.vector._
import osiris.vector.space.VectorSpace

/**
  * Ordinary multiplication of scalars
  */
case class Multiplication[S](space:ScalarSpace[S]) extends BilinearFunction[Unit,Unit,Unit,S] {

  val left   = I --> space
  val right  = I --> space
  val target = I --> space

  override def toString():String = "Mul"

  def serialize:Iterable[Byte] = Iterable(v2.Function.constants.multiplication)

  def predicate(index:(Unit,(Unit,Unit))):Boolean = true

  val predicate:Morphism[(Unit,(Unit,Unit)),bool.BOOL] =
    constant[bool.BOOL](I+I,Right[Unit,Unit]()) << morphism.unit(I*(I*I))

  override def apply(x:Vector[Either[Unit,Unit],S]):Vector[Unit,S] = {
    val xp = x.asPair[Unit,Unit,Either[Unit,Unit]]
    xp.left o xp.right
  }

  def apply[I2,JL2,JR2](iSpace:VectorSpace[I2,S],lSPace:VectorSpace[JL2,S],rSpace:VectorSpace[JR2,S],
                   left:Matrix[Unit,JL2,S],right:Matrix[Unit,JR2,S],
                   op:(Vector[JL2,S],Vector[JR2,S]) => Vector[I2,S]):Matrix[Unit,I2,S] =
    op(left.row(()),right.row(())).toRowVector

  def leftFeedback = this

  def rightFeedback = this

}

object Multiplication {

  def apply[S](space:ScalarSpace[S]):Multiplication[S] = new Multiplication(space)

}


