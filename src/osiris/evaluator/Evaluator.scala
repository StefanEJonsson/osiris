// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.evaluator

import osiris._
import osiris.pin.node.Node
import pin._
import vector._

trait Evaluator {

  def eval(values: Iterable[Pin[_, _]], gradients: Iterable[Pin[_, _]]): Environment

  def values(pins:Pin[_,_]*):Environment = eval(pins,Set())

  def value[I,S](pin:Pin[I,S]):Vector[I,S] = values(pin)(pin)

  def gradients(pins:Pin[_,_]*):Environment = eval(Set(),pins)

  def gradient[I,S](pin:Pin[I,S]):Vector[I,S] = gradients(pin).feedback(pin)

}

trait TwoStepEvaluator extends Evaluator {

  type Computation = Either[Node,Socket[_,_]]
  type Analysis = (collection.mutable.ListBuffer[Computation],collection.mutable.Map[Either[Pin[_,_],Pin[_,_]],Int])

  def eval(values: Iterable[Pin[_, _]], gradients: Iterable[Pin[_, _]]): Environment = {
    compute(analysis(values,gradients))
  }

  def valueDependencies(computation: Computation):Set[Either[Pin[_,_],Pin[_,_]]] = {
    computation match {
      case Left(node) => node.sockets.map(s => Left(s.pin.get))
      case Right(socket) => socket.feedbackDependencies
    }
  }

  def computationDependencies(computation: Computation):Set[Computation] = {
    computation match {
      case Left(node) => node.sockets.map(s => Left(s.pin.get.node))
      case Right(socket) => socket.feedbackDependencies.map {d => d match {
        case Left(pin) => Set(Left(pin.node))
        case Right(pin) => pin.sockets.map(Right(_))
      }}.flatten
    }
  }

  def analysis(values:Iterable[Pin[_,_]],gradients:Iterable[Pin[_,_]]):Analysis

  def compute(analysis: Analysis,environment: Environment = new Environment()):Environment

}

object Evaluator {

  def apply():Evaluator = new BFSAnalysis with SingleThreaded

  def apply(nThreads:Int):Evaluator =
    new BFSAnalysis with MultiThreaded {
      val n:Int = nThreads
      init()
    }

}
