// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node.split

import osiris._
import container.companion.ContainerCompanion
import pin.{MatrixPin, Pin, Socket}
import osiris.evaluator.Environment
import osiris.pin.node.Node
import osiris.pin.node.merge.Merge
import osiris.shape.Shape
import vector.Single
import vector.space.VectorSpace

import scala.collection.mutable

/**
  * Has one socket of vector type and N pins of singleton type. The output of each pin is equal to the corresponding
  * element in the input vector.
  */
class Split[I,S](space:VectorSpace[I,S]) extends Node {

  private val pinSpace:ContainerCompanion[I,Pin[Unit,S]] = space.shape --> [Pin[Unit,S]]()

  private val out = pinSpace(i => new Out(i))

  val pins = out.iterator.toSet //TODO
  val sockets = Set(in)

  def eval(environment: Environment): Unit = {
    space.shape.foreach( i => environment.put(out(i), new Single(environment(in.pin.get)(i)) ))
  }

  def rowWise[II](shape:Shape[II],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[II,_,_]]): Unit = {
    val outer = shape --> space.scalarSpace
    val ss = new RowSplit(space*outer)
    matrixifiedPins(in.pin.get).asInstanceOf[MatrixPin[II,I,S]].transpose ->- ss.in
    for (i <- space.shape) {
      matrixifiedPins(out(i)) = ss(i).toColVector
    }
  }

  object in extends Socket[I,S] {

    val space:VectorSpace[I,S] = Split.this.space

    val node = Split.this

    def evaluateFeedback(environment: Environment): Unit = {
      environment.putFeedback(pin.get,space( (i:I) => environment.feedback(out(i)).asSingle.value))
    }

    def feedbackDependencies = out.iterator.map(Right(_)).toSet

  }

  def apply(i:I):Pin[Unit,S] = out(i)

  class Out(i:I) extends Pin[Unit, S] {

    val space = I --> Split.this.space.scalarSpace

    val node = Split.this

  }

}