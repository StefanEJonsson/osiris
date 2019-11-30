// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.map

import osiris.function.linear.LinearFunction
import osiris.vector.{Matrix, Vector}
import osiris.vector.space.VectorSpace

class RowMappedLinear[I,J1,J2,S](outer:VectorSpace[I,S], f:LinearFunction[J2,J1,S])
  extends LinearFunction[(I,J2),(I,J1),S] {

  val domain = outer * f.domain
  val target = outer * f.target

  def apply(x:Vector[(I,J1),S]):Matrix[I,J2,S] = x.asMatrix.rowMap(f.target,f)

  def feedback = new RowMappedLinear(outer,f.feedback)

}