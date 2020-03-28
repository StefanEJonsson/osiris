// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.vector

import osiris._
import osiris.vector.space.SequentialSpace

trait Sequential[S] extends container.Sequential[S] with Vector[Int,S] {

  override val space:SequentialSpace[S]

  override def asSequential[int <: Int with Int]: Sequential[S] = this

}

class SimpleSequential[S](val space: SequentialSpace[S], v:collection.immutable.Vector[S])
  extends Sequential[S] {

  def serialize: Iterable[Byte] =
    Iterable(utilities.serialization.v2.Vector.elems) ++ iterator.flatMap(space.scalarSpace.serialize(_))

  def apply(i:Int):S = v(i - space.shape.start)

}