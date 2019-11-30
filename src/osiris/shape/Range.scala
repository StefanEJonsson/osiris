// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.shape

import osiris._
import osiris.vector.space.SequentialSpace

class Range (val start:Int, val end:Int) extends Shape[Int] {

  override def size = end - start + 1 //TODO will cause problems if start > end

  override def equals(that:Any):Boolean = that match {
    case (that:Range) => that.start == this.start && that.end == this.end
    case _ => false
  }

  def serialize:Iterable[Byte] =
    Iterable(utilities.Serialization.Shape.range) ++
      utilities.Serialization.Primitives.serializeInt(start) ++
      utilities.Serialization.Primitives.serializeInt(end)

  def deserializeIndex(bytes: Iterator[Byte]): Int = utilities.Serialization.Primitives.deserializeInt(bytes)

  def iterator = (start to end).iterator

  def unary_-() = range(-end,-start)

  def -->[S]():container.companion.SequentialCompanion[S] = new container.companion.SequentialCompanion[S](this)

  def -->[S](s:ScalarSpace[S]):SequentialSpace[S] = new SequentialSpace[S](s,this)

  override def toString: String = s"$start..$end"

}