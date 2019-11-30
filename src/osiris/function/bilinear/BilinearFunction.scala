// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.bilinear

import osiris._
import osiris.function._
import osiris.function.linear.LinearFunction
import osiris.function.linear.reindex.Permute
import osiris.morphism.Isomorphism
import osiris.pin.node.BilinearNode
import osiris.vector.space.VectorSpace
import osiris.vector.{Matrix, Pair, Vector}
import osiris.{I, pin}

trait BilinearFunction[I,L,R,S] extends VectorFunction[I,Either[L,R],S] {

  val left:VectorSpace[L,S]
  val right:VectorSpace[R,S]

  lazy val domain = left + right

  override def apply(x:pin.Pin[Either[L,R],S]):pin.Pin[I,S] = {
    val p = x.asPair[L,R,+[L,R]]
    this(p.left,p.right)
  }

  def apply(l:pin.Pin[L,S],r:pin.Pin[R,S]):pin.Pin[I,S] = {
    if (l.space != left) {
      throw new IllegalArgumentException(s"dimension mismatch: Expected $left got ${l.space}")
    }
    if (r.space != right) {
      throw new IllegalArgumentException(s"dimension mismatch: Expected $right got ${r.space}")
    }
    val c = new BilinearNode(this)
    l ->- c.left
    r ->- c.right
    c.out
  }

  /* ---------------------------------------------------------------------------------------------------------------- */

  def &[I2,L2,R2](that:BilinearFunction[I2,L2,R2,S]):BilinearFunction[(I,I2),(L,L2),(R,R2),S] =
    new LayeredBilinear(this,that)

  def permuteTarget[I2](f:Isomorphism[I2,I]):BilinearFunction[I2,L,R,S] =
    new Permute(f.domain-->scalarSpace,this.target,f) << this

  def permuteLeft[L2](f:Isomorphism[L,L2]):BilinearFunction[I,L2,R,S] =
    this <</ new Permute(left,f.target --> scalarSpace,f)

  def permuteRight[R2](f:Isomorphism[R,R2]):BilinearFunction[I,L,R2,S] =
    this <<\ new Permute(right,f.target --> scalarSpace,f)

  def <</[L2](that:LinearFunction[L,L2,S]):BilinearFunction[I,L2,R,S] =
    new BilinearLeftComposeLinear(this,that)

  def <<\[R2](that:LinearFunction[R,R2,S]):BilinearFunction[I,L,R2,S] =
    new BilinearRightComposeLinear(this,that)

  /* ---------------------------------------------------------------------------------------------------------------- */

  def apply[I2,JL2,JR2](colTarget:VectorSpace[I2,S],colLeft:VectorSpace[JL2,S],colRight:VectorSpace[JR2,S],
                        left:Matrix[L,JL2,S],right:Matrix[R,JR2,S],
                        op:(Vector[JL2,S],Vector[JR2,S])=>Vector[I2,S]):Matrix[I,I2,S]

  def apply(x:Vector[Either[L,R],S]):Vector[I,S] = {
    val xp = x.asPair[L,R,Either[L,R]]
    this.apply[Unit,Unit,Unit](I --> scalarSpace,I --> scalarSpace,I --> scalarSpace,
      xp.left.rows(I --> scalarSpace,s => (I --> scalarSpace)(_ => s)),
      xp.right.rows(I --> scalarSpace,s => (I --> scalarSpace)(_ => s)),
      (l,r) => l o r
    ).rowMap(_.asSingle.value)
  }

  /* ---------------------------------------------------------------------------------------------------------------- */

  def leftFeedback:BilinearFunction[L,I,R,S]

  def rightFeedback:BilinearFunction[R,I,L,S]

  override def feedback(x:Vector[Either[L,R],S],y:Vector[I,S]):Vector[Either[L,R],S] = {
    val xp = x.asPair[L,R,Either[L,R]]
    Pair(leftFeedback(y|xp.right),rightFeedback(y|xp.left))
  }

}
