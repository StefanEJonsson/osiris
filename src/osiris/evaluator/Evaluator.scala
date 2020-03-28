// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.evaluator

import osiris._
import osiris.pin.node.Node
import pin._
import vector._

/**
  * An Evaluator is responsible for evaluating a computation graph. This serves as a base trait for different
  * evaluators.
  */
trait Evaluator {

  def eval(values: Iterable[Pin[_, _]], gradients: Iterable[Pin[_, _]]): Environment

  def values(pins:Pin[_,_]*):Environment = eval(pins,Set())

  def value[I,S](pin:Pin[I,S]):Vector[I,S] = values(pin)(pin)

  def gradients(pins:Pin[_,_]*):Environment = eval(Set(),pins)

  def gradient[I,S](pin:Pin[I,S]):Vector[I,S] = gradients(pin).feedback(pin)

}

object Evaluator {

  def apply():Evaluator = new BFSAnalysis with SingleThreaded

  def apply(nThreads:Int):Evaluator =
    new BFSAnalysis with MultiThreaded {
      val n:Int = nThreads
      init()
    }

}
