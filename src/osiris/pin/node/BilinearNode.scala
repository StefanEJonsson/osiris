// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node

import osiris.function.bilinear.BilinearFunction
import osiris.evaluator.Environment
import osiris.pin.{MatrixPin, Pin, Socket}
import osiris.shape.Shape

import scala.collection.mutable

class BilinearNode[L,R,I,S](f:BilinearFunction[I,L,R,S])
  extends Node {

  val sockets = Set(left,right)
  val pins    = Set(out)

  def eval(environment: osiris.evaluator.Environment): Unit = {
    environment.put(out,f(environment(left.pin.get) | environment(right.pin.get)))
  }

  def rowWise[II](shape:Shape[II],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[II,_,_]]): Unit = {
    val ff = new BilinearNode(new osiris.function.bilinear.RowWiseBilinear(shape-->f.target.scalarSpace,f))
    matrixifiedPins(left.pin.get).asInstanceOf[MatrixPin[II,L,S]] ->- ff.left
    matrixifiedPins(right.pin.get).asInstanceOf[MatrixPin[II,R,S]] ->- ff.right
    matrixifiedPins(out) = ff.out.asMatrix
  }

  object left extends Socket[L,S] {

    val space = f.left
    val node = BilinearNode.this

    def evaluateFeedback(environment: Environment): Unit = {
      environment.putFeedback(pin.get,f.leftFeedback(environment.feedback(out) | environment(right.pin.get)))
    }

    def feedbackDependencies: Set[Either[Pin[_,_], Pin[_,_]]] = Set(Left(right.pin.get),Right(out))

  }

  object right extends Socket[R,S] {

    val space = f.right
    val node = BilinearNode.this

    def evaluateFeedback(environment: Environment): Unit = {
      environment.putFeedback(pin.get,f.rightFeedback(environment.feedback(out) | environment(left.pin.get)))
    }

    def feedbackDependencies: Set[Either[Pin[_,_], Pin[_,_]]] = Set(Left(left.pin.get),Right(out))

  }

  object out extends Pin[I,S] {

    val space = f.target

    val node = BilinearNode.this

  }

}