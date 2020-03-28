// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.container.companion

import osiris._
import container.{Sequential,SimpleSequential}
import shape._

/**
  * Factory object to construct sequential containers.
  *
  * @tparam S the type of elements stored in containers constructed from this object.
  */
class SequentialCompanion[S](override val shape:Range)
  extends ContainerCompanion[Int,S](shape) {

  override def apply(f:Int => S):Sequential[S] = new SimpleSequential(this,shape.map(f).toVector)

}