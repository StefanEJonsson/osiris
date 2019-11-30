// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.container.companion

import osiris.container.Pair

class PairCompanion[L,R,S](val left:ContainerCompanion[L,S], val right:ContainerCompanion[R,S])
  extends ContainerCompanion[Either[L,R],S](left.shape + right.shape) {

  def swap:PairCompanion[R,L,S] = new PairCompanion(right,left)

  def apply(f:Either[L,R]=>S):Pair[L,R,S] = Pair(left( l => f(Left(l))), right( r => f(Right(r))))

}
