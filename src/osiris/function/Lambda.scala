// SPDX-License-Identifier: GPL-2.1
//Copyright © 2020 Stefán E. Jónsson

package osiris.function

import osiris._
import osiris.evaluator.environment.{PinEnvironment, VectorEnvironment}
import osiris.vector.space.VectorSpace
import vector.Vector
import pin.Pin

class Lambda[I,J,S](val domain:VectorSpace[J,S], f:Pin[J,S] => Pin[I,S]) extends VectorFunction[I,J,S] {

  private val result = f(domain.constant.zeros)

  private val in = domain.variable.zeros
  private val out = f(in)

  private val program = new evaluator.compiler.BFSCompiler().compile(Seq(out),Seq())
  private val machine = new evaluator.machine.SingleThreaded

  val target: VectorSpace[I, S] = result.space

  def serialize: Iterable[Byte] = ??? //TODO

  def apply(x: Vector[J, S]): Vector[I, S] = {
    val environment = new VectorEnvironment()
    environment.putValue(in,x)
    machine.run(program,environment)
    environment(out)
  }

  override def apply(x:Pin[J,S]):Pin[I,S] = f(x)

  def feedback: VectorFunction[J, +[J, I], S] = new Lambda[J,+[J,I],S](
    domain + target, x => {
      val xp = x.asPair[J,I,+[J,I]]
      val y = f(xp.left)

      val feedbackProgram = new evaluator.compiler.BFSCompiler().compile(Seq(),Seq(xp.left))

      val environment = new PinEnvironment()
      environment.putFeedback(y,xp.right)

      machine.run(feedbackProgram,environment)

      environment.feedback(xp.left)
    }
  )

}
