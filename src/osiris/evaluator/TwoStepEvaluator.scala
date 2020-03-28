package osiris.evaluator

import osiris.pin.{Pin, Socket}
import osiris.pin.node.Node

/**
  * A base trait for evaluators that do the evaluation in two steps (analysis and computation). The two steps can be
  * thought of as compilation and execution of a program. The analysis results in a list of instructions for how to
  * efficiently evaluate the computation graph. Once a computation graph has been analysed, the same analysis can by
  * used multiple times to perform new computations, saving time in cases where the same computation graph has to be
  * evaluated multiple times.
  */
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
