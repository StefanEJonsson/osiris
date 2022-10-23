// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.evaluator.compiler

import osiris.evaluator.TwoStepEvaluator
import osiris.evaluator.machine.{Instruction, FeedbackInstruction, FinalizeFeedback, NodeInstruction}
import osiris.pin.Pin

/**
  * Does the analysis step of a TwoStepEvaluator using Breadth First Search in the computation graph.
  */
class BFSCompiler extends Compiler {

  def compile(values:Iterable[Pin[_,_]],gradients:Iterable[Pin[_,_]]): Program = {

    val computations = collection.mutable.ArrayBuffer[Instruction]()
    val front = collection.mutable.ListBuffer[Instruction]()
    val memo = collection.mutable.Map[Either[Pin[_,_],Pin[_,_]],Int]()

    values.foreach {v => front.append(NodeInstruction(v.node)); memo.put(Left(v),1)}
    gradients.foreach {g => front.append(FinalizeFeedback(g)); memo.put(Right(g),1)}

    while (front.nonEmpty) {
      val c = front.head
      front.remove(0)
      computations -= c
      computations.prepend(c)
      for (dep <- c.valueDependencies) {
        memo(dep) = memo.getOrElse(dep,0) + 1
      }
      for (dep <- c.computationDependencies) {
        front -= dep
        front.append(dep)
      }
    }
    Program(computations.toArray,memo.toMap)
  }

}
