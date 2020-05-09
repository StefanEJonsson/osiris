// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear

import osiris.function.bilinear.{BilinearFunction, LinearComposeBilinear}
import osiris.{+, pin}
import osiris.function.VectorFunction
import osiris.pin.node.LinearNode
import osiris.vector.Vector

/**
  * Base class for VectorFunctions that are linear ( f(a*x + b*y) = a*f(x) + b*f(y) ). These functions have simpler
  * feedback functions which results in faster backpropagation.
  */
trait LinearFunction[I,J,S] extends VectorFunction[I,J,S] {

  override def apply(x:pin.Pin[J,S]):pin.Pin[I,S] = {
    if (x.space != domain) {
      throw new IllegalArgumentException(s"Dimension mismatch: expected $domain got ${x.space}")
    }
    val c = new LinearNode(this)
    x ->- c.in
    c.out
  }

  def linearFeedback:LinearFunction[J,I,S]

  def feedback:VectorFunction[J,+[J,I],S] = linearFeedback << reindex.Extract.second(domain+target)

  override def feedback(x:Vector[J,S],y:Vector[I,S]):Vector[J,S] = linearFeedback(y)

  def <<[J0](that:LinearFunction[J,J0,S]):LinearFunction[I,J0,S] = new ComposedLinear(this,that)

  def <<[L,R](that:BilinearFunction[J,L,R,S]):BilinearFunction[I,L,R,S] = new LinearComposeBilinear(this,that)

}
