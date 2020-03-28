// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.vector.space

import osiris._
import morphism._
import vector._
import utilities.same

/**
  * In osiris, a matrix is a special type of vector (indexed by the cartesian product of two shapes). Every vector
  * belongs to a vector space. A matrix belongs to a special type of vector space called a matrix space.
  *
  * @param outer the VectorSpace of the columns.
  * @param inner the VectorSpace of the rows.
  */
class MatrixSpace[I,J,S](override val outer:VectorSpace[I,S], override val inner:VectorSpace[J,S])
  extends container.companion.TableCompanion[I,J,S](outer,inner) with VectorSpace[(I,J),S] {

  private val currySpace:container.companion.ContainerCompanion[I,Vector[J,S]] =
    outer.shape --> [Vector[J,S]]()

  val scalarSpace = same(outer.scalarSpace,inner.scalarSpace)

  /* -----------------------------------------------------------------------------------------------------------------*/

  override def transpose:MatrixSpace[J,I,S] = new MatrixSpace(inner,outer)

  /* -----------------------------------------------------------------------------------------------------------------*/

  override def open(file:String):Matrix[I,J,S] = super.open(file).asMatrix


  override def apply(f: ((I, J)) => S): Matrix[I, J, S] =
    new UnCurry(currySpace((i:I) => inner((j:J) => f((i,j)))),inner)

  override def apply(f: ((I,J)) => pin.Pin[Unit,S]):pin.MatrixPin[I,J,S] = super.apply(f).asMatrix

  /* -----------------------------------------------------------------------------------------------------------------*/

  override def fill(elem:S):Matrix[I,J,S] = super.fill(elem).asMatrix

  override def zeros:Matrix[I,J,S] = this.fill(scalarSpace.zero)

  override def  ones:Matrix[I,J,S] = this.fill(scalarSpace.one)

  override def unit(index:(I,J)):Matrix[I,J,S] = super.unit(index).asMatrix

  override def units(predicate: Morphism[(I,J),bool.BOOL]):Matrix[I,J,S] = super.units(predicate).asMatrix

  def identity[IAndJAreTheSame<:I with J]() =
    (new Single(scalarSpace.zero) | new Single(scalarSpace.one)).reIndex[(I,J)](
      bool.equal[I](outer.shape).asInstanceOf[Morphism[(I,J),Either[Unit,Unit]]]
    ).asMatrix

  def rows(f:I => Vector[J,S]):Matrix[I,J,S] =
    new UnCurry(currySpace(
      (i:I) => {
        val row = f(i)
        if (row.space != inner) throw new IllegalArgumentException(s"Dimension mismatch: $row does not fit in $inner")
        row
      }),inner
    )

  def rows(f:I => pin.Pin[J,S]):pin.MatrixPin[I,J,S] = {
    val merge = new osiris.pin.node.merge.RowMerge(this)
    for (i <- outer.shape) {
      f(i) ->- merge.in(i)
    }
    merge.out
  }

  def cols(f:J => Vector[I,S]):Matrix[I,J,S] = this.transpose.rows(f).transpose

  def cols(f:J => pin.Pin[I,S]):pin.MatrixPin[I,J,S] = {
    val merge = new osiris.pin.node.merge.RowMerge(this.transpose)
    for (j <- inner.shape) {
      f(j) ->- merge.in(j)
    }
    merge.out.transpose
  }

  /* ---------------------------------------------------------------------------------------------------------------- */

}