// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.container

import osiris._
import morphism._

/**
  * A set of elements that can be accessed using indices. It can be thought of as a finite mapping
  * from I to S (could be implemented as a hash map).
  *
  * The most important method of a container is the apply method. It inherits this method from Function[I,S]. The apply
  * method takes an index as an argument and returns the element stored at that index.
  *
  * A container has a fixed space. The space of a container specifies which type of elements it can contain and the set
  * of indices that it is defined for. The index set (shape) of a container is always finite. The container is always
  * defined for all the elements of the shape.
  *
  * @tparam I the type of indices used to access elements of the container.
  * @tparam S the type of elements stored in the container.
*/
trait Container[I,S] extends (I => S) {

  /* ---------------------------------------------------------------------------------------------------------------- */

  val space:container.companion.ContainerCompanion[I,S]

  /* ---------------------------------------------------------------------------------------------------------------- */

  /**
    * @return iterator over the elements in the container
    */
  def iterator:Iterator[S] = space.shape.map(this).iterator

  /**
    * Container concatenation (constructs a pair container)
    * @param that the container that is to be concatenated with this container
    * @tparam J the index type to the other container
    * @return a PairContainer consisting of this and that
    */

  def |[J](that:Container[J,S]):Pair[I,J,S] = Pair(this,that)

  /* ---------------------------------------------------------------------------------------------------------------- */

  def reIndex[I0](converter: Morphism[I0,I]): Container[I0,S] =
    new ReIndexed(this,converter)

  /* ---------------------------------------------------------------------------------------------------------------- */

  def asPair[L,R,E<:Either[L,R] with I]:Pair[L,R,S] = this.asInstanceOf[Pair[L,R,S]]

  def asTable[I2,J,P<:(I2,J) with I]:Table[I2,J,S] = this.asInstanceOf[Table[I2,J,S]]

  def asEmpty[n<:Nothing with I]:Empty[S] = this.asInstanceOf[Empty[S]]

  def asSingle[u<:Unit with I]:Single[S] = this.asInstanceOf[Single[S]]

  def asSequential[int<:Int with I]:Sequential[S] = this.asInstanceOf[Sequential[S]]

  /* ---------------------------------------------------------------------------------------------------------------- */

  def toRowVector:Table[Unit,I,S] = this.reIndex[(Unit,I)](product.second(I,space.shape)).asTable

  def toColVector:Table[I,Unit,S] = this.reIndex[(I,Unit)](product.first(space.shape,I)).asTable

}