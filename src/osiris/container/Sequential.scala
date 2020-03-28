// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.container

import osiris.container.companion.SequentialCompanion

/**
  * A container indexed by integers.
  *
  * This is basically what you would call an ordinary "List" or "Vector" with fixed size.
  *
  * @tparam S the type of elements stored in the List.
  */
trait Sequential[S] extends Container[Int,S] {

  override val space:SequentialCompanion[S]

  override def toString():String = this.iterator.mkString("["," ","]")

}

class SimpleSequential[S](val space: SequentialCompanion[S], v:collection.immutable.Vector[S])
  extends Sequential[S] {

  def apply(i:Int):S = v(i - space.shape.start)

}