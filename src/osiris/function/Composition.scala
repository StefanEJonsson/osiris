package osiris.function

import osiris.utilities
import osiris.utilities.serialization.v2
import osiris.vector.Vector

/**
  * The composition of two VectorFunctions.
  *
  * f = g << h <=> f(x) = g(h(x))
  *
  * @param outer left argument for the composition operator (last function to be applied)
  * @param inner right argument for the composition operator (first function to be applied)
  * @tparam I Index type for the output of outer. This will also be the index type for the output of the composition.
  * @tparam K Index type for the intermediate result (output of g and input to f).
  * @tparam J Index type for the input of inner. This will also be the index type for the input of the composition.
  * @tparam S Scalar type.
  */
case class Composition[I,K,J,S](outer:VectorFunction[I,K,S], inner:VectorFunction[K,J,S])
  extends VectorFunction[I,J,S] {

  val domain = inner.domain
  val middle = utilities.same(outer.domain, inner.target)
  val target = outer.target

  override def toString(): String = s"($outer << $inner)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.composition) ++ outer.serialize ++ inner.serialize

  def apply(x: Vector[J, S]): Vector[I, S] = outer(inner(x))

  def feedback(x:Vector[J,S],y:Vector[I,S]):Vector[J,S] = inner.feedback(x,outer.feedback(inner(x),y))

}
