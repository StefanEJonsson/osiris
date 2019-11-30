// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.container

import osiris._
import morphism.absurd
import container.companion.EmptyCompanion

class Empty[S] extends Container[Nothing,S] {

  val space = new EmptyCompanion[S]

  override def toString():String = "()"

  def apply(i:Nothing):S = utilities.absurd(i)

}
