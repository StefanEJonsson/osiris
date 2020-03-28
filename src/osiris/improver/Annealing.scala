// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.improver

import osiris._
import osiris.evaluator.Evaluator
import osiris.vector.{Empty, Single, Vector}
import osiris.vector.space.VectorSpace

/**
  * Reduces risk of getting stuck in local optima by adding random noise to the parameters.
  *
  * This improver only makes sense when added to another improver. The random noise decays exponentially, gradually
  * handing over control to the other improver.
  *
  * @param random random number generator.
  * @param decay rate of decay. Number between zero and one. Close to zero => fast decay. Close to one => slow decay.
  * @param evaluator the evaluator used to compute gradients.
  * @tparam S scalar type.
  */
class Annealing[S](random:Unit => S,decay:S,val evaluator:Evaluator) extends Improver[S] {

  private[improver] type State[P] = Nothing

  private def r() = new Single(random())
  private val d = new Single(decay)

  protected def init[P](space:VectorSpace[P,S]) = new Empty[S](ScalarSpace(decay))

  protected def f[P] = (x:Vector[+[P,Nothing],S],i:Int) =>
    x.space.apply(_ => (r() o (d^i)).asSingle.value)

}
