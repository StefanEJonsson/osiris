// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.improver

import osiris._
import osiris.evaluator.Evaluator
import osiris.vector.space.VectorSpace
import vector.{Empty, Vector}

/**
  * Simple gradient descent.
  * @param rate the learning rate. High values  => fast but has a tendency to overshoot. Low values => slow
  * @param evaluator the evaluator used to compute gradients needed.
  * @tparam S scalar type.
  */
class Vanilla[S](rate:S,val evaluator:Evaluator) extends Improver[S] {

  private[improver] type State[P] = Nothing

  protected def init[P](space:VectorSpace[P,S]) = new Empty[S](ScalarSpace(rate))

  protected def f[P] = (x:Vector[+[P,Nothing],S],_:Int) => x * rate

}
