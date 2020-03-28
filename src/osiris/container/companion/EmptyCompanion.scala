// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.container.companion

import osiris._
import shape._

/**
  * Factory object to construct empty containers.
  *
  * @tparam S the type of elements stored in containers constructed from this object.
  */
class EmptyCompanion[S] extends ContainerCompanion[Nothing,S](Empty) {

  def apply(f:Nothing => S):container.Empty[S] = new container.Empty

  def apply():container.Empty[S] = this(utilities.absurd[S])

}