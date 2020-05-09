// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.evaluator.machine

import osiris.evaluator.compiler.Program
import osiris.evaluator.environment.{Environment, VectorEnvironment}

/**
  * Does the computation step of a TwoStepEvaluator using a single thread.
  */
class SingleThreaded extends Machine {

  def run[Object[_,_]](analysis:Program, environment: Environment[Object]):Unit = {

    val computations = analysis.computations
    val memo = analysis.usages

    for (c <- computations) {
      run(c,environment,collection.mutable.Map(memo.toSeq:_*))
    }
  }

}
