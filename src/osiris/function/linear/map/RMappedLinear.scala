// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.map

import osiris.function.linear.LinearFunction
import osiris.utilities.serialization.v2
import osiris.vector.{Pair, Vector}
import osiris.vector.space.VectorSpace

/**
  * Takes a Pair and produces a new Pair by applying the linear function r on the right component.
  *
  * Note that this does the same thing as osiris.function.map.RMap, but since r is required to be linear, RMappedLinear
  * can also extend LinearFunction.
  *
  * @param l the vector space of the left component.
  * @param r the function to be applied to the right component of the input to produce the right component of the output.
  * @tparam L the index type for the left component.
  * @tparam R1 the index type for the right component of the input.
  * @tparam R2 the index type for the right component of the output.
  * @tparam S the scalar type.
  */
class RMappedLinear[L,R1,R2,S](l:VectorSpace[L,S],r:LinearFunction[R2,R1,S])
  extends LinearFunction[Either[L,R2],Either[L,R1],S] {

  val domain = l + r.domain
  val target = l + r.target

  override def toString():String = s"RMap[$l]($r)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.rMappedLinear) ++
    l.shape.serialize ++ r.serialize

  def apply(x:Vector[Either[L,R1],S]):Pair[L,R2,S] = x.asPair.rmap(r)

  def feedback = new RMappedLinear(l,r.feedback)

}