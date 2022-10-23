// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.evaluator

import osiris.pin.Pin

/**
  * Contains a method for making one computation in a computation graph.
  */
trait Compute extends TwoStepEvaluator {

  def compute(comp:Computation,
              environment: Environment,
              memo:collection.mutable.Map[Either[Pin[_,_],Pin[_,_]],Int]): Unit = {

    comp match {
      case Left(node) => node.eval(environment)
      case Right(socket) => socket.evaluateFeedback(environment)
    }
    for (dep <- valueDependencies(comp)) {
      val remove = memo.synchronized {
        memo(dep) = memo(dep) - 1
        if(memo(dep) == 0) {memo.remove(dep);true} else {false}
      }
      if (remove) {
        dep match {
          case Left(pin) => environment.remove(pin)
          case Right(pin) => environment.removeFeedback(pin)
        }
      }
    }
  }


}
