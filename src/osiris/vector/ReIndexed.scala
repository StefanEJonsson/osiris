// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.vector

import osiris._
import osiris.shape.Shape
import morphism._
import osiris.utilities.serialization
import osiris.utilities.serialization.v2
import osiris.utilities.serialization.Serialization
import osiris.vector.space.VectorSpace

class ReIndexed[I,J,S](v:Vector[J,S],converter:Morphism[I,J])
  extends container.ReIndexed[I,J,S](v,converter) with Vector[I,S] {

  override val space = converter.domain --> v.space.scalarSpace

  //TODO this is not very efficient when v is larger than this
  override def serialize: Iterable[Byte] =
    Iterable(Serialization.version,v2.Vector.reindex) ++
      v.space.shape.serialize ++
      converter.serialize ++
      v.serialize

  override def reIndex[I2](newConverter:Morphism[I2,I]):Vector[I2,S] =
    new ReIndexed[I2,J,S](v,converter << newConverter )

}