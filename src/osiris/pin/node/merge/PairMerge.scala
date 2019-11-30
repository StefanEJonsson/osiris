// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node.merge

import osiris.+
import osiris.pin._
import osiris.evaluator.Environment
import osiris.pin.node.Node
import osiris.pin.node.split.PairSplit
import osiris.shape.Shape
import osiris.vector.space.VectorSpace

import scala.collection.mutable

class PairMerge[L,R,S](lSpace:VectorSpace[L,S], rSpace:VectorSpace[R,S]) extends Node {

  val pins = Set(out)
  val sockets = Set(left,right)

  def eval(environment: Environment): Unit = {
    environment.put(out, environment(left.pin.get) | environment(right.pin.get))
  }

  def rowWise[I](shape:Shape[I],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[I,_,_]]): Unit = {
    val l = matrixifiedPins(left.pin.get).asInstanceOf[MatrixPin[I,L,S]]
    val r = matrixifiedPins(right.pin.get).asInstanceOf[MatrixPin[I,R,S]]
    val oo =
      (l | r).permute(
        osiris.morphism.product.leftDistr(shape,left.space.shape,right.space.shape)
      )
    matrixifiedPins(out) = oo.asMatrix
  }

  object left extends Socket[L,S] {

    val space = lSpace
    val node = PairMerge.this

    def evaluateFeedback(environment: Environment): Unit = {
      environment.putFeedback(pin.get,environment.feedback(out).asPair[L,R,+[L,R]].left)
    }

    def feedbackDependencies = Set(Right(out))

  }

  object right extends Socket[R,S] {

    val space = rSpace
    val node = PairMerge.this

    def evaluateFeedback(environment: Environment): Unit = {
      environment.putFeedback(pin.get,environment.feedback(out).asPair[L,R,+[L,R]].right)
    }

    def feedbackDependencies = Set(Right(out))

  }

  object out extends PairPin[L,R,S] {

    val space = lSpace + rSpace

    val node = PairMerge.this

  }

}