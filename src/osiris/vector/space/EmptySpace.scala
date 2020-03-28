// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.vector.space

import osiris._
import osiris.morphism._
import osiris.vector.Empty

/**
  * The VectorSpace used to construct the empty vector (vector with no indices).
  */
class EmptySpace[S](val scalarSpace: ScalarSpace[S])
  extends container.companion.EmptyCompanion[S] with VectorSpace[Nothing,S] {
  /* ---------------------------------------------------------------------------------------------------------------- */

  override def open(file:String):Empty[S] = super.open(file).asEmpty

  override def apply(f:Nothing => S):Empty[S] = new Empty(scalarSpace)

  override def apply(f:Nothing => pin.Pin[Unit,S]):pin.EmptyPin[S] = super.apply(f).asEmpty

  override def apply():Empty[S] = new Empty(scalarSpace)

  /* ---------------------------------------------------------------------------------------------------------------- */

  override def fill(value:S):Empty[S] = new Empty(scalarSpace)

  override def zeros:Empty[S] = new Empty(scalarSpace)

  override def ones:Empty[S] = new Empty(scalarSpace)

  override def unit(index:Nothing):Empty[S] = utilities.absurd(index)

  override def units(predicate:Morphism[Nothing,Either[Unit,Unit]]):Empty[S] =
    new Empty(scalarSpace)

}