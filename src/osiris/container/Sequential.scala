// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.container

import osiris.container.companion.SequentialCompanion

trait Sequential[S] extends Container[Int,S] {

  override val space:SequentialCompanion[S]

  override def toString():String = this.iterator.mkString("["," ","]")

}

class SimpleSequential[S](val space: SequentialCompanion[S], v:collection.immutable.Vector[S])
  extends Sequential[S] {

  def apply(i:Int):S = v(i - space.shape.start)

}