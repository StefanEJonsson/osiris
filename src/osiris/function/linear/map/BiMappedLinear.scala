// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.map

import osiris.function.linear.LinearFunction
import osiris.utilities.serialization.v2
import osiris.vector.{Pair, Vector}

/**
  * Transforms a Pair to a new Pair by applying the linear function l on the left component and the linear function r on
  * the right component.
  *
  * Note that this function does the same thing as osiris.function.map.BiMap, but since l and r are required to be
  * linear, BiMappedLinear can also extend LinearFunction.
  *
  * @param l function to be applied on the left component of the input to produce the left component of the output.
  * @param r function to be applied on the right component of the input to produce the right component of the output.
  * @tparam L1 index type for the left component of the input.
  * @tparam L2 index type for the left component of the output.
  * @tparam R1 index type for the right component of the input.
  * @tparam R2 index type for the right component of the output.
  * @tparam S scalar type.
  */
class BiMappedLinear[L1,L2,R1,R2,S](l:LinearFunction[L2,L1,S],r:LinearFunction[R2,R1,S])
  extends LinearFunction[Either[L2,R2],Either[L1,R1],S] {

  val domain = l.domain + r.domain
  val target = l.target + r.target

  override def toString():String = s"BiMap($l,$r)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.biMappedLinear) ++
    l.serialize ++ r.serialize

  def apply(x:Vector[Either[L1,R1],S]):Pair[L2,R2,S] = x.asPair.bimap(l,r)

  def linearFeedback = new BiMappedLinear(l.linearFeedback,r.linearFeedback)

}