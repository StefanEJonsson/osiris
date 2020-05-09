// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.evaluator.environment

import osiris.evaluator.machine.{Instruction, FeedbackInstruction, FinalizeFeedback, NodeInstruction}
import osiris.pin._
import osiris.vector.space.VectorSpace
import osiris.vector.{Empty, Matrix, Pair, Sequential, Single, Vector}

/**
  * Contains a mapping between pins and values (and also a mapping between pins and gradients).
  *
  * It is used by Evaluators to keep track of intermediate results when evaluating a computation graph. Because every
  * evaluator creates its own environment, it is possible for multiple evaluators to work simultaneously on the same
  * graph.
  *
  */
class VectorEnvironment extends Environment[Vector] {

  protected def zeros[I,S](space:VectorSpace[I,S]):Vector[I,S] = space.zeros

  protected def add[I,S](a:Vector[I,S],b:Vector[I,S]):Vector[I,S] = a + b

  def run(computation: Instruction): Unit = computation match {
    case FinalizeFeedback(pin) => finalizeFeedback(pin)
    case NodeInstruction(node) => node.eval(this)
    case FeedbackInstruction(socket) => socket.evaluateFeedback(this)
  }

  def apply[I,S](pin:Pin[I,S]):Vector[I,S] = value(pin)

  def apply[S](pin:EmptyPin[S]):EmptyPin[S] = value[Nothing,S](pin).asInstanceOf[EmptyPin[S]]

  def apply[S](pin:SinglePin[S]):Single[S] = value(pin).asInstanceOf[Single[S]]

  def apply[S](pin:SequentialPin[S]):Sequential[S] = value(pin).asInstanceOf[Sequential[S]]

  def apply[L,R,S](pin:PairPin[L,R,S]):Pair[L,R,S] = value(pin).asInstanceOf[Pair[L,R,S]]

  def apply[I,J,S](pin:MatrixPin[I,J,S]):Matrix[I,J,S] = value(pin).asInstanceOf[Matrix[I,J,S]]

}
