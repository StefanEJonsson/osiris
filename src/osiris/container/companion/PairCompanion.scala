// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.container.companion

import osiris.container.Pair

/**
  * Factory object to construct pairs of containers.
  *
  * @tparam L the type of indices to the left container
  * @tparam R the type of indices to the right container
  * @tparam S the type of elements stored in containers constructed from this object.
  */
class PairCompanion[L,R,S](val left:ContainerCompanion[L,S], val right:ContainerCompanion[R,S])
  extends ContainerCompanion[Either[L,R],S](left.shape + right.shape) {

  def swap:PairCompanion[R,L,S] = new PairCompanion(right,left)

  def apply(f:Either[L,R]=>S):Pair[L,R,S] = Pair(left( l => f(Left(l))), right( r => f(Right(r))))

}
