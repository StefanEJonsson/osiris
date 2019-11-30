// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear

import osiris.vector.{Matrix, Vector}

class SimpleLinear[I,J,S](val k:Matrix[I,J,S]) extends LinearFunction[I,J,S] {

  val domain = k.inner
  val target = k.outer

  def apply(x:Vector[J,S]):Vector[I,S] = k*x

  def feedback = new SimpleLinear(k.transpose)

  def <<[J0](that:SimpleLinear[J,J0,S]):SimpleLinear[I,J0,S] = new SimpleLinear(this.k * that.k)

}
