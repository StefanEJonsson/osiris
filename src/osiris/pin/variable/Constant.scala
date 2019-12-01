// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.variable

import osiris._
import osiris.morphism.{Morphism, bool}
import osiris.pin.{MatrixPin, Pin}
import osiris.evaluator.Environment
import osiris.pin.node.Node
import osiris.shape.Shape
import osiris.vector._
import osiris.vector.space.{MatrixSpace, VectorSpace}

import scala.collection.mutable

class Constant[I,S] (value:Vector[I,S]) extends Pin[I,S] {

  val space = value.space

  val node = new Node {

    val sockets = Set()
    val pins = Set(Constant.this)

    def eval(environment: Environment): Unit = {
      environment.put(Constant.this,value)
    }

    def rowWise[II](shape:Shape[II],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[II,_,_]]): Unit = {
      matrixifiedPins(Constant.this) =
        new function.linear.reindex.RowCopy(shape-->space.scalarSpace,space)(Constant.this).asMatrix
    }

  }

  override def toString():String = space match {
    case (_:MatrixSpace[_,_,S]) => s"constant =\n $value"
    case _ => s"Constant = $value"
  }

}


class ConstantSpace[I,S](vectorSpace: VectorSpace[I,S]) {

  /* ---------------------------------------------------------------------------------------------------------------- */

  def apply(f:I=>S):Constant[I,S] = new Constant[I,S](vectorSpace.apply(f))

  /* ---------------------------------------------------------------------------------------------------------------- */

  def fill(s:S):Constant[I,S] = new Constant[I,S](vectorSpace.fill(s))

  def zeros:Constant[I,S] = new Constant[I,S](vectorSpace.zeros)

  def ones:Constant[I,S] = new Constant[I,S](vectorSpace.ones)

  def unit(i:I):Constant[I,S] = new Constant[I,S](vectorSpace.unit(i))

  def units(predicate:Morphism[I,bool.BOOL]):Constant[I,S] = new Constant[I,S](vectorSpace.units(predicate))

}
