// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.vector

import osiris._

class Empty[S] (scalarSpace: ScalarSpace[S]) extends container.Empty[S] with Vector[Nothing,S] {

  override val space = O --> scalarSpace

  override def asEmpty[n <: Nothing with Nothing]: Empty[S] = this

}
