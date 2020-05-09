// SPDX-License-Identifier: GPL-2.1
//Copyright © 2020 Stefán E. Jónsson

package osiris.evaluator

import osiris.evaluator.compiler.{Compiler, Program}
import osiris.evaluator.machine.Machine
import osiris.evaluator.environment.VectorEnvironment
import osiris.pin.Pin

class StaticEvaluator(compiler:Compiler,machine:Machine) extends Evaluator {

  private val memo = collection.mutable.Map[(Iterable[Pin[_,_]],Iterable[Pin[_,_]]),Program]()

  def eval(values: Iterable[Pin[_, _]], gradients: Iterable[Pin[_, _]]): VectorEnvironment = {
    val environment = new VectorEnvironment
    if (memo.contains(values,gradients)) {
      machine.run(memo((values,gradients)),environment)
    } else {
      val prog = compiler.compile(values,gradients)
      memo.put((values,gradients),prog)
      machine.run(prog,environment)
    }
    environment
  }

}
