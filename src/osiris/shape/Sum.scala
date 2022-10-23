// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.shape

import osiris.utilities.serialization
import osiris.utilities.serialization.v2
import osiris.utilities.serialization.Serialization
import osiris.{ScalarSpace, container, utilities}
import osiris.vector.space.PairSpace

/**
  * The tagged union of two shapes.
  */
class Sum[I,J] (val a:Shape[I],val b:Shape[J]) extends Shape[Either[I,J]] {

  override def size = a.size + b.size

  override def toString():String = s"($a + $b)"

  override def equals(that:Any):Boolean = that match {
    case (that:Sum[I,J]) =>  that.a == this.a && that.b == this.b
    case _ => false
  }

  def serialize:Iterable[Byte] = Iterable(v2.Shape.sum) ++ a.serialize ++ b.serialize

  def deserializeIndex(bytes:Iterator[Byte]):Either[I,J] = {
    val h = bytes.next()
    if (h == v2.Index.left) {
      Left(a.deserializeIndex(bytes))
    } else if (h == v2.Index.right) {
      Right(b.deserializeIndex(bytes))
    } else {
      throw new IllegalArgumentException(s"$h is neither left nor right")
    }
  }

  def iterator = a.iterator.map(Left(_)) ++ b.iterator.map(Right(_))

  def -->[S]():container.companion.PairCompanion[I,J,S] =
    new container.companion.PairCompanion[I,J,S](a-->[S](),b-->[S]())

  def -->[S](s:ScalarSpace[S]):PairSpace[I,J,S] = new PairSpace(a-->s,b-->s)

}
