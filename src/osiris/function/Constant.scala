// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function

import osiris._
import vector._

class Constant[I,S](c:Vector[I,S]) extends VectorFunction[I,Nothing,S] {

  val target = c.space
  val domain = O --> target.scalarSpace

  override def toString():String = s"Constant $c"

  def apply(x:Vector[Nothing,S]): Vector[I,S] = c

  def feedback(x:Vector[Nothing,S],y:Vector[I,S]):Vector[Nothing,S] = new Empty[S](scalarSpace)

}
