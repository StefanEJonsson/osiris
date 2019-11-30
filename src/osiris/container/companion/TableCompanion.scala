// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.container.companion

import osiris.container.Table
import osiris.container._

class TableCompanion[I,J,S](val outer:ContainerCompanion[I,S], val inner:ContainerCompanion[J,S])
  extends ContainerCompanion[(I,J),S](outer.shape*inner.shape) {

  private val currySpace:ContainerCompanion[I,Container[J,S]] = outer.shape --> [Container[J,S]]()

  def transpose:TableCompanion[J,I,S] = new TableCompanion(inner,outer)

  def apply(f:((I,J))=>S):Table[I,J,S] = new UnCurry(currySpace(i => inner(j => f(i,j))),inner)

  def rows(f:I => Container[J,S]):Table[I,J,S] = new UnCurry(currySpace(i => f(i)),inner)

  def cols(f:J => Container[I,S]):Table[I,J,S] = transpose.rows(f).transpose

}
