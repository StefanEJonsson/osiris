// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin.node.replace

import osiris.evaluator.Environment
import osiris.pin.{MatrixPin, Pin, Socket}
import osiris.pin.node.Node
import osiris.shape.Shape
import osiris.vector.space.MatrixSpace

import scala.collection.mutable

class ReplaceRow[I,J,S](space:MatrixSpace[I,J,S],i:I) extends Node {

  val sockets = Set(in,replacement)
  val pins = Set(out)

  def eval(environment: Environment): Unit = {
    val value = environment(replacement.pin.get)
    val res = environment(in.pin.get).asMatrix.replaceRow(i,value)
    environment.put(out,res)
  }

  def rowWise[II](shape:Shape[II],matrixifiedPins:mutable.Map[Pin[_,_],MatrixPin[II,_,_]]): Unit = {
    import osiris.morphism._
    val rr = new ReplaceRow(space.outer*(space.inner * (shape-->space.scalarSpace)),i)
    val min =
      matrixifiedPins(in.pin.get).asInstanceOf[MatrixPin[II,(I,J),S]]permute(
        product.commute(space.shape,shape) o
        product.assocLeft(space.outer.shape,space.inner.shape,shape)
      )
    val mrep = matrixifiedPins(replacement.pin.get).asInstanceOf[MatrixPin[II,J,S]]
    min ->- rr.in
    mrep.transpose ->- rr.replacement
    matrixifiedPins(out) =
      rr.out.permute(
        product.assocRight(space.outer.shape,space.inner.shape,shape) o
        product.commute(shape,space.shape)
      ).asMatrix[II,(I,J),(II,(I,J))]
  }

  object in extends Socket[(I,J),S] {

    val space = ReplaceRow.this.space
    val node = ReplaceRow.this

    def evaluateFeedback(environment: Environment): Unit = {
      val feedback = environment.feedback(out).asMatrix.replaceRow(i,space.inner.zeros)
      environment.putFeedback(pin.get,feedback)
    }

    def feedbackDependencies: Set[Either[Pin[_,_],Pin[_,_]]] = Set(Right(out))

  }

  object replacement extends Socket[J,S] {

    val space = ReplaceRow.this.space.inner
    val node = ReplaceRow.this

    def evaluateFeedback(environment: Environment): Unit = {
      val feedback = environment.feedback(out).asMatrix[I,J,(I,J)].row(i)
      environment.putFeedback(pin.get,feedback)
    }

    def feedbackDependencies: Set[Either[Pin[_,_],Pin[_,_]]] = Set(Right(out))

  }

  object out extends MatrixPin[I,J,S] {

    val space = ReplaceRow.this.space

    val node = ReplaceRow.this

  }

}
