// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin

import osiris._
import function.{ScalarFunction, VectorFunction}
import function.bilinear.Multiplication
import vector.{Vector,Single}
import vector.space.SingleSpace

trait SinglePin[S] extends Pin[Unit,S] {

  val space:SingleSpace[S]

  override def map(f:ScalarFunction[S]):SinglePin[S] = super.map(f).asSingle

  override def elemWise(op:VectorFunction[Unit,Either[Unit,Unit],S])(that:Pin[Unit,S]):SinglePin[S] =
    super.elemWise(op)(that).asSingle

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def *(k:SinglePin[S]):SinglePin[S] = Multiplication(space.scalarSpace)(this|k).asSingle

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def +(that:Pin[Unit,S]):SinglePin[S] = super.+(that).asSingle

  override def -(that:Pin[Unit,S]):SinglePin[S] = super.-(that).asSingle

  override def unary_-():SinglePin[S] = super.unary_-().asSingle

  override def o(that:Pin[Unit,S]):SinglePin[S] = Multiplication(space.scalarSpace)(this|that).asSingle

  override def asSingle[u<:Unit with Unit]:SinglePin[S] = this

}
