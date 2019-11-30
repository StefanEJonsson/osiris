// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.container

import osiris._
import container.companion.SingleCompanion

class Single[S](val value:S) extends Container[Unit,S] {

  val space:SingleCompanion[S] = new SingleCompanion[S]

  override def toString():String = s"($value)"

  def apply(i:Unit):S = value

}