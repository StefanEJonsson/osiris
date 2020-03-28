// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node

import osiris.function.VectorFunction
import osiris.evaluator.Environment
import osiris.pin.{MatrixPin, Pin, Socket}
import osiris.shape.Shape

/**
  * A [Node] where the computation is given by a [VectorFunction].
  */
class FunctionNode[I,J,S](f:VectorFunction[I,J,S]) extends Node {

  val sockets = Set(in)
  val pins = Set(out)

  def eval(environment: osiris.evaluator.Environment): Unit = {
    environment.put(out,f(environment(in.pin.get)))
  }

  def rowWise[II](shape:Shape[II],matrixifiedPins:collection.mutable.Map[Pin[_,_],MatrixPin[II,_,_]]): Unit = {
    val ff = new FunctionNode(new osiris.function.map.RowMap(shape-->out.space.scalarSpace,f))
    matrixifiedPins(in.pin.get).asInstanceOf[MatrixPin[II,J,S]] ->- ff.in
    matrixifiedPins(out) = ff.out.asMatrix
  }

  object in extends Socket[J,S] {

    val space = f.domain
    val node = FunctionNode.this

    def evaluateFeedback(environment: Environment): Unit = {
      environment.putFeedback(pin.get,f.feedback(environment(pin.get),environment.feedback(out)))
    }

    def feedbackDependencies: Set[Either[Pin[_,_], Pin[_,_]]] = Set(Left(in.pin.get),Right(out))

  }

  object out extends Pin[I,S] {

    val space = f.target

    val node = FunctionNode.this

  }

}
