// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function

import osiris._
import vector.Vector
import vector.space.VectorSpace

class DeadInput[J,S](val domain:VectorSpace[J,S]) extends VectorFunction[Nothing,J,S] {

  val target = O --> domain.scalarSpace

  override def toString():String = s"DeadInput $domain"

  def apply(x: Vector[J, S]): Vector[Nothing, S] = target((i:Nothing) => utilities.absurd(i))

  def feedback(x:Vector[J,S],y:Vector[Nothing,S]) = domain.zeros

}