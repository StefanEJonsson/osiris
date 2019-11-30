// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.container

import osiris._
import morphism._
import container.companion._
import osiris.shape.Shape

trait Container[I,S] extends (I => S) {

  /* ---------------------------------------------------------------------------------------------------------------- */

  val space:container.companion.ContainerCompanion[I,S]

  /* ---------------------------------------------------------------------------------------------------------------- */

  def iterator:Iterator[S] = space.shape.map(this).iterator

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

  def toRowVector:Table[Unit,I,S] = this.reIndex[(Unit,I)](product.right(I,space.shape)).asTable

  def toColVector:Table[I,Unit,S] = this.reIndex[(I,Unit)](product.left(space.shape,I)).asTable

}