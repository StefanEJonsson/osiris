// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node

import osiris.evaluator.Environment
import osiris.pin.{MatrixPin, Pin, Socket}
import osiris.shape.Shape
import osiris.vector.space.VectorSpace

import scala.collection.mutable

/**
  * Works like identity function during forward propagation but multiplies feedback by minus one during back
  * propagation. The resulting behaviour is that all parameters that are used before this node are adjusted to optimize
  * the opposite of the objective functions that occur after this node.
  *
  * This is useful when building things like for example Generative Adverserial Network. In order to do that you would
  * first construct the generator network. Let us call the output of the generator y. Then you would write:
  *
  * val adv = new Adverserial(y.space)
  * y ->- adv.in
  * val y2 = adv.out
  *
  * Now you can feed y2 into your discriminator. The objective function of the discriminator will be to guess correctly
  * as often as possible. The objective of the generator will therefore automatically become to make the discriminator
  * guess incorrectly as often as possible.
  */
class Adversarial[I,S](space:VectorSpace[I,S]) extends Node {

  val sockets = Set(in)
  val pins = Set(out)

  def eval(environment: osiris.evaluator.Environment): Unit = {
    environment.put(out,environment(in.pin.get))
  }

  def rowWise[II](shape:Shape[II],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[II,_,_]]):Unit = {
    val AA = new Adversarial(shape*space.shape --> space.scalarSpace)
    matrixifiedPins(in.pin.get).asInstanceOf[MatrixPin[II,I,S]] ->- AA.in
    matrixifiedPins(out) = AA.out.asMatrix
  }

  object in extends Socket[I,S] {

    val space:VectorSpace[I,S] = Adversarial.this.space
    val node = Adversarial.this

    def evaluateFeedback(environment: Environment): Unit = {
      environment.putFeedback(pin.get,-environment.feedback(out))
    }

    def feedbackDependencies:Set[Either[Pin[_,_], Pin[_,_]]] = Set(Right(out))

  }

  object out extends Pin[I,S] {

    val space = Adversarial.this.space

    val node = Adversarial.this

  }

}
