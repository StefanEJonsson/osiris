// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.improver

import osiris._
import osiris.evaluator.Evaluator
import osiris.vector.space.VectorSpace
import osiris.vector.{Single, Vector}

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
