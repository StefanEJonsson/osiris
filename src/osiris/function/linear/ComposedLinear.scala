// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear

import osiris.vector.Vector

class ComposedLinear[I,K,J,S](outer:LinearFunction[I,K,S],inner:LinearFunction[K,J,S])
  extends LinearFunction[I,J,S] {

  val domain = inner.domain
  val target = outer.target

  def apply(x:Vector[J,S]):Vector[I,S] = outer(inner(x))

  def feedback = inner.feedback << outer.feedback

}
