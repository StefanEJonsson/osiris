// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.shape

import osiris.{ScalarSpace, container, utilities}
import osiris.vector.space.MatrixSpace

class Product[I,J] (val a:Shape[I],val b:Shape[J]) extends Shape[(I,J)] {

  override def size = a.size * b.size

  override def toString():String = s"($a * $b)"

  override def equals(that:Any):Boolean = that match {
    case (that:Product[I,J]) => that.a == this.a && that.b == this.b
    case _ => false
  }

  def serialize:Iterable[Byte] = Iterable(utilities.Serialization.Shape.product) ++ a.serialize ++ b.serialize

  def deserializeIndex(bytes:Iterator[Byte]):(I,J) = {
    val i = a.deserializeIndex(bytes)
    val j = b.deserializeIndex(bytes)
    (i,j)
  }

  def iterator = a.iterator.flatMap(i => b.iterator.map { j => (i,j)})

  def -->[S]():container.companion.TableCompanion[I,J,S] = a.table(b-->[S]())

  def -->[S](s:ScalarSpace[S]):MatrixSpace[I,J,S] = a-->(b-->s)

}
