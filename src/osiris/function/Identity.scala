// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function

import osiris._
import vector._
import space.VectorSpace

class Identity[I,S](domain:VectorSpace[I,S])
  extends function.linear.reindex.Permute[I,I,S](domain,domain,morphism.id(domain.shape)) {

  override def apply(x:Vector[I,S]):Vector[I,S] = x

  override val feedback = this

}
