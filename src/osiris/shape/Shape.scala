// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.shape

import osiris._
import osiris.container.companion.ContainerCompanion
import osiris.morphism.{Isomorphism, Monomorphism, Morphism}
import osiris.utilities.serialization
import osiris.utilities.serialization.v2
import osiris.utilities.serialization.v2.{Primitives}
import osiris.vector.space._

/**
  * A shape is simply a finite set. Every vector has a shape. The elements of the shape are the elements used as indices
  * for the vector.
  *
  * @tparam I the index type.
  */
trait Shape[I] extends Iterable[I] {

  type Type = I

  def serialize:Iterable[Byte]

  def deserializeIndex(bytes:Iterator[Byte]):I

  def iterator:Iterator[I]

  /**
    * The tagged union of two shapes.
    */
  def +[J](that:Shape[J]):Sum[I,J] = new Sum(this,that)

  /**
    * The cartesian product of two shapes.
    */
  def *[J](that:Shape[J]):Product[I,J] = new Product(this,that)

  /**
    * Used to create a [osiris.container.companion.ContainerCompanion] that is used to construct Containers that are
    * indexed by elements of this shape and contain elements of type S.
    */
  def -->[S]():ContainerCompanion[I,S]
  def table[J,S](c:container.companion.ContainerCompanion[J,S]):container.companion.TableCompanion[I,J,S] =
    new container.companion.TableCompanion(this --> [S](),c)

  /**
    * Constructs the [osiris.vector.space.VectorSpace] representing vectors that are indexed by this shape and contain
    * scalars from the scalar field s.
    */
  def -->[S](s:ScalarSpace[S]):VectorSpace[I,S]

  /**
    * Constructs the [osiris.vector.space.MatrixSpace] representing matrices whose rows are indexed by this shape and
    * where each row is a vector in the VectorSpace s.
    */
  def -->[J,S](s:VectorSpace[J,S]):MatrixSpace[I,J,S] =
    new MatrixSpace(this --> s.scalarSpace,s)

}