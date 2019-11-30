// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.container.companion

import osiris.{container, _}
import shape._

class SingleCompanion[S] extends ContainerCompanion[Unit,S](Single) {

  override def apply(f:Unit => S):container.Single[S] = new container.Single(f())

}