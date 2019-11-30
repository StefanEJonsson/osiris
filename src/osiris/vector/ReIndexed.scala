// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.vector

import osiris._
import osiris.shape.Shape
import morphism._
import osiris.vector.space.VectorSpace

class ReIndexed[I,J,S](v:Vector[J,S],converter:Morphism[I,J])
  extends container.ReIndexed[I,J,S](v,converter) with Vector[I,S] {

  override val space = converter.domain --> v.space.scalarSpace

  override def serialize: Iterable[Byte] =
    Iterable(utilities.Serialization.version,utilities.Serialization.Representation.reindex) ++
      v.space.shape.serialize ++
      converter.serialize ++
      v.serialize

  override def reIndex[I2](newConverter:Morphism[I2,I]):Vector[I2,S] =
    new ReIndexed[I2,J,S](v,converter << newConverter )

}