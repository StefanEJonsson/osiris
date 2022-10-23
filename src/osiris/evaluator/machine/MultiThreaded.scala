// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.evaluator.machine

import osiris.evaluator.compiler.Program
import osiris.evaluator.environment.{Environment, VectorEnvironment}
import osiris.pin.Pin

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Does the computation step of a TwoStepEvaluator using n threads.
 */
class MultiThreaded(n:Int) extends Machine {

  def run[Object[_,_]](analysis: Program, environment: Environment[Object]): Unit =  {
    new Execution[Object](
      ArrayBuffer(analysis.computations:_*),
      collection.mutable.Map(analysis.usages.toSeq:_*),
      environment
    )
  }

  private class Execution[Object[_,_]](computations:collection.mutable.ArrayBuffer[Instruction],
                                       usages:collection.mutable.Map[Either[Pin[_,_],Pin[_,_]],Int],
                                       environment:Environment[Object]) {

    private val workers = (0 until n).map( i => new Worker)
    workers.foreach(w => w.join())

    private def takeWork():Option[Instruction] = computations.synchronized {
      if (computations.isEmpty) {
        Option.empty
      } else {
        val res = computations(0)
        computations.remove(0)
        Option(res)
      }
    }

    var i = 0

    class Worker extends Thread {

      val n = i.synchronized { i += 1; i}

      start()

      override def run(): Unit = {
        try {

          var comp = takeWork()
          while (comp.isDefined) {
            MultiThreaded.this.run(comp.get,environment,usages,n)
            comp = takeWork()
          }

        } catch {
          case (_:InterruptedException) => {}
        }
      }

    }

  }

}
