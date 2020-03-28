// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.reindex

import osiris.ScalarSpace
import osiris.function.linear.LinearFunction
import osiris.morphism._
import osiris.vector._

abstract class ReIndex[I,J,S](scalarSpace:ScalarSpace[S],
                              val transformation:Morphism[I,J]) extends LinearFunction[I,J,S] {

  val domain = transformation.target --> scalarSpace
  val target = transformation.domain --> scalarSpace

  override def toString():String = s"ReIndex($transformation)"

  def apply(x:Vector[J,S]):Vector[I,S] = x.reIndex(target,transformation)

}





