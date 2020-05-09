// SPDX-License-Identifier: GPL-2.1
//Copyright © 2020 Stefán E. Jónsson

package osiris.evaluator

import osiris.evaluator.compiler.Compiler
import osiris.evaluator.machine.Machine
import osiris.evaluator.environment.VectorEnvironment
import osiris.pin.Pin

/**
  * A base trait for evaluators that do the evaluation in two steps (analysis and computation). The two steps can be
  * thought of as compilation and execution of a program. The analysis results in a list of instructions for how to
  * efficiently evaluate the computation graph. Once a computation graph has been analysed, the same analysis can by
  * used multiple times to perform new computations, saving time in cases where the same computation graph has to be
  * evaluated multiple times.
  */
class TwoStepEvaluator(compiler:Compiler,machine:Machine) extends Evaluator {

  def eval(values: Iterable[Pin[_, _]], gradients: Iterable[Pin[_, _]]): VectorEnvironment = {
    val environment = new VectorEnvironment
    machine.run(compiler.compile(values,gradients),environment)
    environment
  }

}
