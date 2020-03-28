// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear.reindex

import osiris._
import osiris.morphism.Monomorphism
import osiris.utilities.serialization.v2
import osiris.vector.space.{MatrixSpace, PairSpace, VectorSpace}

/**
  * Takes a vector x and produces a new vector y that contains some subset of the elements of x. The subset is
  * determined by f:
  *
  * y(i) = x(f(i))
  *
  * @param s the scalar space.
  * @param f the function that defines the relationship between indices in the output and input vectors.
  * @tparam A the index type for the output.
  * @tparam B the index type for the input.
  * @tparam S the scalar type.
  */
class Extract[A,B,S](s:ScalarSpace[S],f:Monomorphism[A,B])
  extends ReIndex(s,f) {

  override def toString():String = s"Extract($f)"

  def serialize:Iterable[Byte] =
    Iterable(v2.Function.constants.extract) ++ f.serialize

  def feedback:Pad[B,A,S] = new Pad(s,f)

}

object Extract {

  /**
    * The function that takes a vector x and returns the singleton (x(i))
    */
  def element[I,S](domain:VectorSpace[I,S], i:I) =
    new Extract(domain.scalarSpace,morphism.constant(domain.shape,i))

  /**
    * The function that takes a pair (x,y) and returns the left component x.
    */
  def first[L,R,S](domain:PairSpace[L,R,S]) =
    new Extract[L,+[L,R],S](domain.scalarSpace,morphism.sum.left(domain.left.shape,domain.right.shape))

  /**
    * The function that takes a pair (x,y) and returns the right component y.
    */
  def second[L,R,S](domain:PairSpace[L,R,S]) =
    new Extract[R,+[L,R],S](domain.scalarSpace,morphism.sum.right(domain.left.shape,domain.right.shape))

  /**
    * The function that takes a matrix and returns the ith row.
    */
  def row[I,J,S](domain:MatrixSpace[I,J,S], i:I) =
    new Extract[J,*[I,J],S](domain.scalarSpace,morphism.product.rightPairedWith(domain.outer.shape,domain.inner.shape,i))

  /**
    * The function that takes a matrix and returns the jth column.
    */
  def col[I,J,S](domain:MatrixSpace[I,J,S], j:J) =
    new Extract[I,*[I,J],S](domain.scalarSpace,morphism.product.leftPairedWith(domain.outer.shape,domain.inner.shape,j))


}
