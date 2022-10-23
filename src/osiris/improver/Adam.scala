// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.improver

import osiris._
import osiris.evaluator.Evaluator
import osiris.vector.space.VectorSpace
import osiris.vector.{Single, Vector}

/**
  * Adaptive Moment estimating Gradient Descent
  *
  * An improved version of the gradient descent technique for optimization that speeds up convergence by keeping track
  * of the first- and (unbalanced) second moment of the gradient.
  *
  * @param rate the learning rate determines the speed of the optimization. Too high and it will overshoot a lot. Too
  *             low and the learning will be unnecessarily slow.
  * @param decay1 a number between zero and one determining the rate of decay of the first moment. Numbers close to zero
  *               result in fast decay and numbers close to one result in slow decay.
  *
  *               Values around 0.9 are recommended.
  *
  *               Intuitively, decay1 can be thought of as the memory of the improver. If it is close to one, the
  *               improver "remembers" a large portion of the past gradients as it moves on and continues to be
  *               influenced by them.
  * @param decay2 a number between zero and one determining the rate of decay of the second moment. Numbers close to
  *               zero give fast decay and numbers close to one give slow decay.
  *
  *               Values around 0.999 are recommended.
  * @param epsilon a small constant that is used to avoid division by zero.
  * @param evaluator the evaluator used to compute the gradients needed by the improver.
  * @tparam S the scalar type
  */
class Adam[S](rate:S,decay1:S,decay2:S,epsilon:S,val evaluator:Evaluator)
  extends Improver[S] {

  val sspace:ScalarSpace[S] = ScalarSpace(rate)
  val space = I --> sspace

  private val d1 = new Single(decay1)
  private val d2 = new Single(decay2)

  private[improver] type State[P] = Either[P,P]

  protected def init[P](space:VectorSpace[P,S]) = space.zeros | space.zeros

  protected def f[P] = (x:Vector[+[P,+[P,P]],S],i:Int) => {
    val xp = x.asPair[P,+[P,P],+[P,+[P,P]]]
    val grad = xp.left
    val xr = xp.right.asPair[P,P,+[P,P]]
    val mean = xr.left
    val variance = xr.right

    val newMean     = mean*decay1 + grad * (space.ones - d1)
    val newVariance = variance*decay2 + (grad o grad)*(space.ones - d2)

    val correctedMean     = newMean * (space.ones - (d1 ^ i)).inv
    val correctedVariance = newVariance * (space.ones - (d2^i)).inv

    val update = correctedMean o
      (correctedVariance.map(v => sspace.inv(sspace.+(sspace.^(v,sspace.fromDouble(0.5)),epsilon)))) * rate

    update | (newMean | newVariance)
  }

}
