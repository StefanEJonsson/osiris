// SPDX-License-Identifier: GPL-2.1
//Copyright © 2020 Stefán E. Jónsson

package osiris.evaluator.machine

import osiris.evaluator.compiler.Program
import osiris.evaluator.environment.Environment
import osiris.pin.Pin

trait Machine {

  def run[Object[_,_]](analysis: Program, environment: Environment[Object]):Unit

  protected def run[Object[_,_]](computation:Instruction,
                                 environment:Environment[Object],
                                 memo:collection.mutable.Map[Either[Pin[_,_],Pin[_,_]],Int], n:Int = 0):Unit = {

    environment.run(computation)

    for (dep <- computation.valueDependencies) {
      val remove = memo.synchronized {
        memo(dep) = memo(dep) - 1
        if(memo(dep) == 0) {memo.remove(dep);true} else {false}
      }
      if (remove) {
        dep match {
          case Left(pin) => environment.removeValue(pin)
          case Right(pin) => environment.removeFeedback(pin)
        }
      }
    }
  }

}
