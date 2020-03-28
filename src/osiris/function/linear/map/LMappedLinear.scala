// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.map

import osiris.function.linear.LinearFunction
import osiris.utilities.serialization.v2
import osiris.vector.{Pair, Vector}
import osiris.vector.space.VectorSpace

/**
  * Takes a Pair and produces a new Pair by applying the linear function l on the left component.
  *
  * Note that this does the same thing as osiris.function.map.LMap, but since l is required to be linear, LMappedLinear
  * can also extend LinearFunction.
  *
  * @param l the function to be applied to the left component of the input to produce the left component of the output.
  * @param r the vector space of the right component.
  * @tparam L1 the index type for the left component of the input.
  * @tparam L2 the index type for the left component of the output.
  * @tparam R the index type for the right component.
  * @tparam S the scalar type.
  */
class LMappedLinear[L1,L2,R,S](l:LinearFunction[L2,L1,S],r:VectorSpace[R,S])
  extends LinearFunction[Either[L2,R],Either[L1,R],S] {

  val domain = l.domain + r
  val target = l.target + r

  override def toString():String = s"LMap[$r]($l)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.lMappedLinear) ++
    l.serialize ++ r.shape.serialize

  def apply(x:Vector[Either[L1,R],S]):Pair[L2,R,S] = x.asPair.lmap(l)

  def feedback = new LMappedLinear(l.feedback,r)


}
