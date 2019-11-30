// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.map

import osiris.function.linear.LinearFunction
import osiris.vector.{Matrix, Vector}
import osiris.vector.space.VectorSpace

class ColMappedLinear[I1,I2,J,S](inner:VectorSpace[J,S], f:LinearFunction[I2,I1,S])
  extends LinearFunction[(I2,J),(I1,J),S] {

  val domain = f.domain * inner
  val target = f.target * inner

  def apply(x:Vector[(I1,J),S]):Matrix[I2,J,S] = x.asMatrix.colMap(f.target,f)

  def feedback = new ColMappedLinear(inner,f.feedback)

}
