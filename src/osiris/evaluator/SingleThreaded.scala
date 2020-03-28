// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.evaluator

/**
  * Does the computation step of a TwoStepEvaluator using a single thread.
  */
trait SingleThreaded extends Compute {

  def compute(analysis:Analysis,environment: Environment = new Environment()):Environment = {

    val computations = analysis._1
    val memo = analysis._2

    for (c <- computations) {
      compute(c,environment,memo)
    }

    environment
  }

}
