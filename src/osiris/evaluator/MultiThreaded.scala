// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.evaluator

import osiris.pin.Pin

/**
  * Does the computation step of a TwoStepEvaluator using n threads.
 */
trait MultiThreaded extends Compute {

  val n:Int

  def init():Unit = (0 until n).foreach {_ => val w = new Worker(); w.start()}

  private var computations = Option.empty[collection.mutable.ListBuffer[Computation]]
  private var memo = Option.empty[collection.mutable.Map[Either[Pin[_,_],Pin[_,_]],Int]]
  private var environment = Option.empty[Environment]

  private var done = Option.empty[collection.mutable.Set[Computation]]

  private val compLock = new Object()
  private val doneLock = new Object()

  def compute(analysis: Analysis, environment: Environment): Environment =  {
    val n = compLock.synchronized {
      computations = Option(analysis._1)
      memo = Option(analysis._2)
      this.environment = Option(environment)
      done = Option(collection.mutable.Set())
      compLock.notifyAll()
      computations.get.size
    }
    doneLock.synchronized { while (done.get.size != n) {doneLock.wait()} }
    environment
  }

  private def takeWork():Computation = compLock.synchronized {
    while (computations.forall(_.isEmpty)) {
      compLock.wait()
    }
    val c = computations.get.head
    computations.get.remove(0)
    return c
  }

  class Worker extends Thread {

    override def run(): Unit = {
      try {
        while (true) {
          val comp = takeWork()
          for (dep <- computationDependencies(comp)) {
            val d:Object = dep match {
              case Left(d) => d
              case Right(d) => d
            }
            d.synchronized {
              while(doneLock.synchronized {!done.get.contains(dep)} ) {
                d.wait()
              }
            }
          }
          compute(comp,environment.get,memo.get)
          val c = comp match {
            case Left(c) => c
            case Right(c) => c
          }
          c.synchronized {
            doneLock.synchronized {
              done.get += comp
              doneLock.notifyAll()
            }
            c.notifyAll()
          }
        }
      } catch {
        case (_:InterruptedException) => {}
      }
    }

  }



}
