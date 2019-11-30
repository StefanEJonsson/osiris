// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.evaluator

import osiris.pin.Pin

trait BFSAnalysis extends TwoStepEvaluator {

  def analysis(values:Iterable[Pin[_,_]],gradients:Iterable[Pin[_,_]]): Analysis = {

    val computations = collection.mutable.ListBuffer[Computation]()
    val front = collection.mutable.ListBuffer[Computation]()
    val memo = collection.mutable.Map[Either[Pin[_,_],Pin[_,_]],Int]()

    values.foreach {v => front.append(Left(v.node)); memo.put(Left(v),1)}
    gradients.foreach {g => g.sockets.foreach(s => front.append(Right(s))); memo.put(Right(g),1)}

    while (front.nonEmpty) {
      val c = front.head
      front.remove(0)
      computations -= c
      computations.prepend(c)
      for (dep <- valueDependencies(c)) {
        memo(dep) = memo.getOrElse(dep,0) + 1
      }
      for (dep <- computationDependencies(c)) {
        front -= dep
        front.append(dep)
      }
    }
    (computations,memo)
  }

}
