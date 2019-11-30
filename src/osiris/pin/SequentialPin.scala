// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin

import osiris.function.{ScalarFunction, VectorFunction}
import osiris.vector.space.SequentialSpace

trait SequentialPin[S] extends Pin[Int,S] {

  val space:SequentialSpace[S]

  override def map(f:ScalarFunction[S]):SequentialPin[S] = super.map(f).asSequential

  override def elemWise(op:VectorFunction[Unit,Either[Unit,Unit],S])(that:Pin[Int,S]):SequentialPin[S] =
    super.elemWise(op)(that).asSequential

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def *(k:SinglePin[S]):SequentialPin[S] = super.*(k).asSequential

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def +(that:Pin[Int,S]):SequentialPin[S] = super.+(that).asSequential

  override def -(that:Pin[Int,S]):SequentialPin[S] = super.-(that).asSequential

  override def unary_-():SequentialPin[S] = super.unary_-().asSequential

  override def o(that:Pin[Int,S]):SequentialPin[S] = super.o(that).asSequential

}