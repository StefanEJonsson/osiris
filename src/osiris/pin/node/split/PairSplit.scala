// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node.split

import osiris._
import pin.node.Node
import pin.node.merge.PairMerge
import pin.{MatrixPin, Pin, Socket}
import osiris.evaluator.Environment
import osiris.shape.Shape
import vector.space.VectorSpace

import scala.collection.mutable

class PairSplit[L,R,S](lSpace:VectorSpace[L,S], rSpace:VectorSpace[R,S]) extends Node {

  val pins = Set(left,right)
  val sockets = Set(in)

  def eval(environment: Environment): Unit = {
    environment.put(left , environment(in.pin.get).asPair[L,R,+[L,R]].left )
    environment.put(right, environment(in.pin.get).asPair[L,R,+[L,R]].right)
  }

  def rowWise[I](shape:Shape[I],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[I,_,_]]): Unit = {
    import morphism._
    val outer = shape --> lSpace.scalarSpace
    val ss = new PairSplit(outer*lSpace,outer*rSpace)
    val min =
      matrixifiedPins(in.pin.get).asInstanceOf[MatrixPin[I,Either[L,R],S]].permute(
        product.leftExtract(shape,lSpace.shape,rSpace.shape)
      ).asPair[(I,L),(I,R),+[(I,L),(I,R)]]
    min ->- ss.in
    matrixifiedPins(left) = ss.left.asMatrix[I,L,(I,L)]
    matrixifiedPins(right) = ss.right.asMatrix[I,R,(I,R)]
  }

  object in extends Socket[Either[L,R],S] {

    val space = lSpace + rSpace
    val node = PairSplit.this

    def evaluateFeedback(environment: Environment): Unit = {
      environment.putFeedback(pin.get,environment.feedback(left) | environment.feedback(right))
    }

    def feedbackDependencies = Set(Right(left),Right(right))

  }

  object left extends Pin[L,S] {

    val space = lSpace

    val node = PairSplit.this

  }

  object right extends Pin[R,S] {

    val space = rSpace

    val node = PairSplit.this

  }

}


