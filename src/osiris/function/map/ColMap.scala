// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.map

import osiris.function.VectorFunction
import osiris.vector.Vector
import osiris.vector.space.VectorSpace

class ColMap[I1,I2,J,S](inner:VectorSpace[J,S],f:VectorFunction[I2,I1,S])
  extends VectorFunction[(I2,J),(I1,J),S] {

  val domain = f.domain * inner
  val target = f.target * inner

  def apply(x:Vector[(I1,J),S]):Vector[(I2,J),S] = x.asMatrix.colMap(f.target,f)

  def feedback(x:Vector[(I1,J),S],y:Vector[(I2,J),S]):Vector[(I1,J),S] = {
    val xm = x.asMatrix[I1,J,(I1,J)]
    val ym = y.asMatrix[I2,J,(I2,J)]
    xm.colWise(f.domain,(x:Vector[I1,S],y:Vector[I2,S]) => f.feedback(x,y))(ym)
  }

}
