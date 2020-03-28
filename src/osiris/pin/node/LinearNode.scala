// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node

import osiris.function.linear.LinearFunction
import osiris.evaluator.Environment
import osiris.pin.{MatrixPin, Pin, Socket}
import osiris.shape.Shape

import scala.collection.mutable

/**
  * Like a [FunctionNode] but with the restriction that the function must be linear. This makes the feedback computation
  * a bit more efficient.
  */
class LinearNode[I,J,S](f:LinearFunction[I,J,S]) extends Node {

  val sockets = Set(in)
  val pins = Set(out)

  def eval(environment: Environment): Unit = {
    environment.put(out,f(environment(in.pin.get)))
  }

  def rowWise[II](shape:Shape[II],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[II,_,_]]): Unit = {
    val ff = new LinearNode(new osiris.function.linear.map.RowMappedLinear(shape-->out.space.scalarSpace,f))
    matrixifiedPins(in.pin.get).asInstanceOf[MatrixPin[II,J,S]] ->- ff.in
    matrixifiedPins(out) = ff.out.asMatrix
  }

  object in extends Socket[J,S] {

    val space = f.domain
    val node = LinearNode.this

    def evaluateFeedback(environment: Environment): Unit = {
      environment.putFeedback(pin.get,f.feedback(environment.feedback(out)))
    }

    def feedbackDependencies = Set(Right(out))

  }

  object out extends Pin[I,S] {

    val space = f.target

    val node = LinearNode.this

  }

}