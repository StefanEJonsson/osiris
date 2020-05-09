// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.map

import osiris._
import osiris.function.{Lambda, VectorFunction}
import osiris.morphism.{product, sum}
import osiris.utilities.serialization.v2
import osiris.vector.Vector
import osiris.vector.space.VectorSpace

/**
  * Takes a pair of vectors and produces a new vector by applying the binary operation f on matching elements of the
  * vectors.
  *
  * z(i) = f(x(i),y(i))
  */
class ElemWise[I,S](val target:VectorSpace[I,S],op:VectorFunction[Unit,Either[Unit,Unit],S])
  extends VectorFunction[I,Either[I,I],S] {

  val domain = target + target

  override def toString():String = s"ElemWise[$target]($op)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.elemWise) ++
    target.shape.serialize ++ op.serialize

  private def pair(x: S, y: S): Vector[Either[Unit, Unit], S] = ((I + I) --> scalarSpace) {
    case Left(()) => x
    case Right(()) => y
  }

  def apply(x: Vector[Either[I, I], S]): Vector[I, S] = {
    val xp = x.asPair[I,I,Either[I,I]]
    xp.left.elemWise((x, y) => op(pair(x, y))(()))(xp.right) //feels a bit complicated
  }

  def feedback:VectorFunction[+[I,I],+[+[I,I],I],S] = new Lambda(
    domain + target, x => {

      val xp = x.asPair[+[I,I],I,+[+[I,I],I]]

      val input = xp.left.asPair[I,I,+[I,I]]
      val feed = xp.right

      val opfeed = op.feedback

      val tupleSpace = (osiris.I + osiris.I) --> target.scalarSpace
      val matrix = (target * tupleSpace).rows ( (i:I) => opfeed((input.left(i) | input.right(i)) | feed(i)) )

      val columns = matrix.permute(product.leftExtract(target.shape,osiris.I,osiris.I))

      columns.permute(sum.bimap(product.putLeft(target.shape),product.putLeft(target.shape)))
  })

}