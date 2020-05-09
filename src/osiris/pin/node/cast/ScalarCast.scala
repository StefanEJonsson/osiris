// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node.cast

import osiris.ScalarSpace
import osiris.evaluator.environment.VectorEnvironment
import osiris.pin.{MatrixPin, Pin, Socket}
import osiris.pin.node.Node
import osiris.shape.Shape

import scala.collection.mutable

class ScalarCast[I,S1,S2](val shape:Shape[I],
                          val from:ScalarSpace[S1],
                          val to:ScalarSpace[S2],
                          val cast:S1=>S2,
                          val inverseCast:S2=>S1) extends Node {

  val sockets = Set(in)
  val pins = Set(out)

  def eval(environment:VectorEnvironment): Unit = {
    val x = environment(in.pin.get)
    val y = (x.space.shape  --> to)((i:I) => cast(x(i)))
    environment.putValue(out,y)
  }

  def rowWise[I](shape:Shape[I],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[I,_,_]]): Unit = {
    matrixifiedPins(out) = matrixifiedPins(in.pin.get)
  }

  object in extends Socket[I,S1] {

    val space = shape --> from
    val node = ScalarCast.this

    def evaluateFeedback(environment: VectorEnvironment): Unit = {
      val x = environment.feedback(out)
      val y = space((i:I) => inverseCast(x(i)))
      environment.putFeedback(in.pin.get,y)
    }

    def feedbackDependencies: Set[Either[Pin[_, _], Pin[_,_]]] = Set(Right(out))

  }

  object out extends Pin[I,S2] {

    val space = shape --> to

    val node = ScalarCast.this

  }

}
