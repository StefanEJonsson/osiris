// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.map

import osiris.function.{ScalarFunction, VectorFunction}
import osiris.utilities.serialization.v2
import osiris.vector._
import osiris.vector.space.VectorSpace

/**
  * Takes a vector and produces a new vector by applying the function f to every element of the vector.
  *
  * y(i) = f(x(i))
  *
  * @param domain the vector space that input (and output) vectors belong to.
  * @param f function to be applied to elements of the vector.
  *
  */
class Map[I,S](val domain:VectorSpace[I,S], f:ScalarFunction[S])
  extends VectorFunction[I,I,S] {

  val target = domain

  override def toString():String = s"Map[$domain]($f)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.map) ++
    domain.shape.serialize ++ f.serialize

  def apply(x:Vector[I,S]):Vector[I,S] = x.map(f)

  def feedback(x:Vector[I,S],y:Vector[I,S]):Vector[I,S] = new Map(domain,f.derivative)(x) o y

}

