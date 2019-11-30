// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.map

import osiris.function.VectorFunction
import osiris.vector.Vector
import osiris.vector.space.VectorSpace

class RowMap[I,J1,J2,S](outer:VectorSpace[I,S],f:VectorFunction[J2,J1,S])
  extends VectorFunction[(I,J2),(I,J1),S] {

  val domain = outer * f.domain
  val target = outer * f.target

  def apply(x:Vector[(I,J1),S]):Vector[(I,J2),S] = x.asMatrix.rowMap(f.target,f)

  def feedback(x:Vector[(I,J1),S],y:Vector[(I,J2),S]):Vector[(I,J1),S] = {
    val xm = x.asMatrix[I,J1,(I,J1)]
    val ym = y.asMatrix[I,J2,(I,J2)]
    xm.rowWise(f.domain,(x:Vector[J1,S],y:Vector[J2,S]) => f.feedback(x,y))(ym)
  }

}