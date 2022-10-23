// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin

import osiris.function.{ScalarFunction, VectorFunction}
import osiris.vector.space.EmptySpace

trait EmptyPin[S] extends Pin[Nothing,S] {

  val space:EmptySpace[S]

  override def map(f:ScalarFunction[S]):EmptyPin[S] = this

  override def elemWise(op:VectorFunction[Unit,Either[Unit,Unit],S])(that:Pin[Nothing,S]):EmptyPin[S] = this

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def *(k:SinglePin[S]):EmptyPin[S] = this

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def +(that:Pin[Nothing,S]):EmptyPin[S] = this

  override def -(that:Pin[Nothing,S]):EmptyPin[S] = this

  override def unary_-():EmptyPin[S] = this

  override def o(that:Pin[Nothing,S]):EmptyPin[S] = this

  override def asEmpty[n <: Nothing with Nothing]: EmptyPin[S] = this

}

