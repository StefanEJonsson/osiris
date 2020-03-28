// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.container

import osiris._
import container.companion.EmptyCompanion

/**
  * A container containing no elements.
  *
  * The shape of this container is the empty set.
  *
  * @tparam S the type of elements (not) stored in the container.
  */
class Empty[S] extends Container[Nothing,S] {

  val space = new EmptyCompanion[S]

  override def toString():String = "()"

  def apply(i:Nothing):S = utilities.absurd(i)

}
