// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.variable

import osiris._
import osiris.evaluator.environment.VectorEnvironment
import osiris.morphism.{Morphism, bool}
import osiris.pin.{MatrixPin, Pin}
import osiris.pin.node.Node
import osiris.shape.Shape
import osiris.vector._
import osiris.vector.space.{MatrixSpace, VectorSpace}

import scala.collection.mutable

/**
  * A pin that represents a mutable value. It starts with an initial value. It can then be changed by the user. It does
  * not adjust itself automatically during optimization like a [Parameter] does.
  */
class Variable[I,S](val init:Vector[I,S]) extends Pin[I,S] {

  val space = init.space

  private var data = init

  val node = new Node {

    val sockets = Set()
    val pins = Set(Variable.this)

    def eval(environment: VectorEnvironment): Unit = {
      environment.putValue(Variable.this,data)
    }

    def rowWise[II](shape:Shape[II],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[II,_,_]]): Unit = {
      matrixifiedPins(Variable.this) =
        new function.linear.reindex.RowCopy(shape-->space.scalarSpace,space)(Variable.this).asMatrix
    }

  }

  override def toString():String = space match {
    case (_:MatrixSpace[_,_,S]) => s"Variable =\n $value"
    case _ => s"Variable = $value"
  }

  def set(value:Vector[I,S]): Unit = {
    data = value
  }

  def value = data

}

class VariableSpace[I,S](vectorSpace: VectorSpace[I,S]) {

  /* ---------------------------------------------------------------------------------------------------------------- */

  def apply(f:I=>S):Variable[I,S] = new Variable[I,S](vectorSpace.apply(f))

  /* ---------------------------------------------------------------------------------------------------------------- */

  def fill(s:S):Variable[I,S] = new Variable[I,S](vectorSpace.fill(s))

  def zeros:Variable[I,S] = new Variable[I,S](vectorSpace.zeros)

  def ones:Variable[I,S] = new Variable[I,S](vectorSpace.ones)

  def unit(i:I):Variable[I,S] = new Variable[I,S](vectorSpace.unit(i))

  def units(predicate:Morphism[I,bool.BOOL]):Variable[I,S] = new Variable[I,S](vectorSpace.units(predicate))

}
