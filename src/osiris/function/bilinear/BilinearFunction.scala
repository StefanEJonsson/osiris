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

/**
  * Base trait for bilinear functions.
  *
  * Bilinear functions are functions that depend on two vector arguments and are linear with respect to one argument
  * when the other is held constant and vice versa. These functions are very commonly used in machine learning and they
  * have gradients that are easy to compute.
  *
  * @tparam I The index type of the vectors returned from the function
  * @tparam L The index type of the vectors used as the first argument to the function
  * @tparam R The index type of the vectors used as the second argument to the function
  * @tparam S The scalar field
  */
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

  /**
    * Computes the tensor product of this BilinearFunction with that BilinearFunction.
    *
    * This is for example used to construct the operator used for convolutional layers (which can be described as the
    * tensor product of matrix multiplication with convolution).
    *
    * @param that BilinearFunction to be combined with this BilinearFunction to produce a more complex BilinearFunction.
    * @tparam I2 Index type of the vectors returned by the other BilinearFunction.
    * @tparam L2 Index type of the first argument of the other BilinearFunction.
    * @tparam R2 Index type of the second argument of the other BilinearFunction.
    */
  def &[I2,L2,R2](that:BilinearFunction[I2,L2,R2,S]):BilinearFunction[(I,I2),(L,L2),(R,R2),S] =
    new LayeredBilinear(this,that)

  /**
    * Constructs a new BilinearFunction that does the same thing as this BilinearFunction and then applies a permutation
    * to the resulting vector.
    *
    * @param f the permutation to be applied
    * @tparam I2 the index type of the resulting vectors from the new BilinearFunction
    */
  def permuteTarget[I2](f:Isomorphism[I2,I]):BilinearFunction[I2,L,R,S] =
    new Permute(scalarSpace,f) << this

  /**
    * Constructs a new BilinearFunction that does the same thing as this BilinearFunction but first applies a
    * permutation to the first argument.
    *
    * @param f the permutation to be applied
    * @tparam L2 the index type of the first argument to the new BilinearFunction
    */
  def permuteLeft[L2](f:Isomorphism[L,L2]):BilinearFunction[I,L2,R,S] =
    this <</ new Permute(scalarSpace,f)

  /**
    * Constructs a new BilinearFunction that does the same thing as this BilinearFunction but first applies a
    * permutation to the second argument.
    *
    * @param f the permutation to be applied
    * @tparam R2 the index type of the second argument to the new BilinearFunction
    */
  def permuteRight[R2](f:Isomorphism[R,R2]):BilinearFunction[I,L,R2,S] =
    this <<\ new Permute(scalarSpace,f)

  /**
    * Constructs a new BilinearFunction that does the same thing as this BilinearFunction but first applies a
    * linear transformation to the first argument.
    *
    * @param that the linear transformation to be applied
    * @tparam L2 the index type of the first argument to the new BilinearFunction
    */
  def <</[L2](that:LinearFunction[L,L2,S]):BilinearFunction[I,L2,R,S] =
    new BilinearLeftComposeLinear(this,that)

  /**
    * Constructs a new BilinearFunction that does the same thing as this BilinearFunction but first applies a
    * linear transformation to the second argument.
    *
    * @param that the linear transformation to be applied
    * @tparam R2 the index type of the second argument to the new BilinearFunction
    */
  def <<\[R2](that:LinearFunction[R,R2,S]):BilinearFunction[I,L,R2,S] =
    new BilinearRightComposeLinear(this,that)

  /* ---------------------------------------------------------------------------------------------------------------- */


  //this method will probably be removed in the future. I think there is a much better solution.
  /**
    * Computes the tensor product of this BilinearFunction with op and applies the resulting operator on left|right.
    *
    * When you implement this method in a subclass you should think about it as if you are implementing the ordinary
    * apply function for your BilinearFunction, but everywhere where you expect there to be a scalar there is a vector
    * instead and everywhere where you would normally use multiplication you should use op instead.
    *
    * @param colTarget The target of op.
    * @param colLeft The space of the first argument if op.
    * @param colRight The space of the second argument of op.
    * @param left The first argument
    * @param right The second argument
    * @param op
    * @tparam I2 Index type of vectors returned by op
    * @tparam JL2 Index type of vectors used as first argument for op
    * @tparam JR2 Index type of vectors used as second argument for op.
    */
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

  /**
    * A function that computes the feedback to the left argument, given the right argument along with the feedback to
    * output of this.
    *
    * Note that since this is linear with respect to the left argument when the right argument is kept constant, the
    * derivative of this with respect to the left argument is constant with respect to the left argument. It is still
    * linear with respect to the right argument. The feedback function, which is the product of the derivative of this
    * with the feedback to output of this, does therefore not depend on the left argument. Furthermore it is linear with
    * respect to the feedback to the output of this when the right argument is kept constant and it is linear with
    * respect to the right argument when the feedback to the output of this is kept constant. This is why the resulting
    * function is bilinear.
    */
  def leftFeedback:BilinearFunction[L,I,R,S]

  /**
    * A function that computes the feedback to the right argument, given the left argument and the feedback to the
    * output of this.
    *
    * Note that since this is linear with respect to the right argument when the left argument is kept constant, the
    * derivative of this with respect to the right argument is constant with respect to the right argument. It is still
    * linear with respect to the left argument. The feedback function, which is the product of the derivative of this
    * with the feedback to output of this, does therefore not depend on the right argument. Furthermore it is linear
    * with respect to the feedback to the output of this when the left argument is kept constant and it is linear with
    * respect to the left argument when the feedback to the output of this is kept constant. This is why the resulting
    * function is bilinear.
    *
    */
  def rightFeedback:BilinearFunction[R,I,L,S]

  override def feedback:VectorFunction[+[L,R],+[+[L,R],I],S] = new Lambda(
    domain + target, x => {
    val xp = x.asPair[+[L,R],I,+[+[L,R],I]]
    val input = xp.left.asPair[L,R,+[L,R]]
    val feed = xp.right
    leftFeedback(feed | input.right) | rightFeedback(feed | input.left)
  })

}
