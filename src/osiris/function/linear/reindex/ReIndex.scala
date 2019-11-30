// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.reindex

import osiris.function.linear.LinearFunction
import osiris.morphism._
import osiris.vector._
import osiris.vector.space._

abstract class ReIndex[I,J,S](val target:VectorSpace[I,S],
                                         val domain:VectorSpace[J,S],
                                         val transformation:Morphism[I,J]) extends LinearFunction[I,J,S] {

  def apply(x:Vector[J,S]):Vector[I,S] = x.reIndex(target,transformation)

}





