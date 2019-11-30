// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.variable

import osiris.pin.{MatrixPin, Pin}
import osiris.vector._
import java.io.File

import osiris.morphism.{Morphism, bool}
import osiris.pin.node.Node
import osiris.evaluator.Environment
import osiris.function
import osiris.shape.Shape
import osiris.vector.space.{EmptySpace, VectorSpace}

import scala.collection.mutable

class Parameter[I,S](val name:String, init:Vector[I,S])
  extends Pin[I,S] {

  val space = init.space

  private var param =
    if(new File(name).exists()) {
      println(s"opened parameter $name")
      space.open(name)
    } else {
      println(s"new parameter $name")
      init
    }

  val node = new Node {

    val sockets = Set()
    val pins = Set(Parameter.this)

    def eval(environment: Environment): Unit = {
      environment.put(Parameter.this,param)
    }

    def rowWise[II](shape:Shape[II],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[II,_,_]]): Unit = {
      matrixifiedPins(Parameter.this) =
        new function.linear.reindex.RowCopy(shape-->space.scalarSpace,space)(Parameter.this).asMatrix
    }

  }

  override def toString():String = s"Parameter $name = $param"

  def set(value:Vector[I,S]): Unit = synchronized {
    param = value
  }

  def get():Vector[I,S] = synchronized {param}

  def add(grad:Vector[I,S]): Unit = synchronized {
    set(get() + grad)
  }

  def save(): Unit = synchronized {
    param.save(name)
  }

  def size:Int = space.shape.size

}

class ParameterSpace[I,S](name:String,vectorSpace: VectorSpace[I,S]) {

  /* ---------------------------------------------------------------------------------------------------------------- */

  def apply(f:I=>S):Parameter[I,S] = new Parameter(name,vectorSpace.apply(f))

  /* ---------------------------------------------------------------------------------------------------------------- */

  def fill(s:S):Parameter[I,S] = new Parameter(name,vectorSpace.fill(s))

  def zeros:Parameter[I,S] = new Parameter(name,vectorSpace.zeros)

  def ones:Parameter[I,S] = new Parameter(name,vectorSpace.ones)

  def unit(i:I):Parameter[I,S] = new Parameter(name,vectorSpace.unit(i))

  def units(predicate:Morphism[I,bool.BOOL]):Parameter[I,S] = new Parameter(name,vectorSpace.units(predicate))

}
