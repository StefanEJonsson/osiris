// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.container.companion

import osiris._
import osiris.shape.Shape

/**
  * Base class for factory objects for [[osiris.container.Container]].
  *
  * @tparam I the type of indices used to access elements in containers constructed from this object.
  * @tparam S the type of elements stored in containers constructed from this object.
  *
  * @constructor Create new ContainerCompanion for containers with indices in shape.
  * @param shape the set of indices used to access elements in containers constructed from this object.
  *
 */
abstract class ContainerCompanion[I,S](val shape:Shape[I]) {

  /* ---------------------------------------------------------------------------------------------------------------- */

  /**
    * Constructs a container with f(i) as its element at index i for every i in shape.
    *
    * @param f a function or lambda expression specifying the elements of the container at every given index.
   */
  def apply(f:I=>S):container.Container[I,S]

  /* ---------------------------------------------------------------------------------------------------------------- */

  def +[J](that:ContainerCompanion[J,S]):PairCompanion[I,J,S] = new PairCompanion(this,that)

  def *[J](that:ContainerCompanion[J,S]):TableCompanion[I,J,S] = new TableCompanion(this,that)

  /* ---------------------------------------------------------------------------------------------------------------- */

  def asPair[L,R,E<:Either[L,R] with I]:PairCompanion[L,R,S] = this.asInstanceOf[PairCompanion[L,R,S]]

  def asTable[Outer,Inner,P<:(Outer,Inner) with I]:TableCompanion[Outer,Inner,S] =
    this.asInstanceOf[TableCompanion[Outer,Inner,S]]

  def asEmpty[n<:Nothing with I]:EmptyCompanion[S] = this.asInstanceOf[EmptyCompanion[S]]

  def asSingle[u<:Unit with I]:SingleCompanion[S] = this.asInstanceOf[SingleCompanion[S]]

  def asSequential[int<:Int with I]:SequentialCompanion[S] = this.asInstanceOf[SequentialCompanion[S]]

  /* ---------------------------------------------------------------------------------------------------------------- */

}