// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.improver

import osiris._
import osiris.pin.Socket
import osiris.evaluator.Environment
import osiris.pin.variable.Parameter
import osiris.utilities.serialization
import osiris.utilities.serialization.v2
import osiris.utilities.serialization.Serialization
import osiris.utilities.serialization.v2.Primitives
import osiris.vector.space.VectorSpace
import vector.Vector

/**
  * Base class for improvers.
  *
  * Improvers are optimization algorithms that improve a set of parameters by changing them in small steps usually with
  * some gradient method. The most important function is the step function. It applies one step of the optimization
  * algorithm to the parameters provided as argument.
  *
  * "Improve" in this context means increasing the value of the objective variables in the computation graph that the
  * parameters belong to. If o1 is an [[osiris.Objective]] with strength s1, o2 is an objective with strength s2 and
  * o3 is an objective with strength s3, then the variable u = s1*o1 + s2*o2 + s3*o3 is the maximized by the improver.
  * Every step should try to increase the value u by a small amount. Optimization is achieved by calling step many
  * times.
  *
  * @tparam S
  */

trait Improver[S] {

  private val outer = this

  /**
    * Has to be specified by the concrete improver.
    *
    * P represents the index type of a parameter to be adjusted by the improver. For every P, State[P] should be the
    * index type of the internal state of the improver when working with such a parameter.
    *
    * If the improver has no internal state then State[P] = Nothing.
    *
    * If the improver has an internal state that always has the same shape as the parameter to be improved then
    * State[P] = P.
    *
    * If the improver has an internal state that consists of two vectors, both being of the same shape as the vector
    * to be improved, then State[P] = Either[P,P].
    *
    * If the improver has an integer indexed vector of internal states for each scalar parameter to be improved, then
    * State[P] = (P,Int).
    */
  private[improver] type State[P]

  protected val evaluator:osiris.evaluator.Evaluator

  /**
    * Has an OptimizationSession for each parameter encountered by the improver. This ensures that when the improver
    * improves a parameter that has already been improved earlier, the internal state that was created in the last step
    * is used in the new step.
    */
  private val parameters = collection.mutable.Map[Parameter[_,S],OptimizationSession[_,_,S]]()

  /**
    * Has to be specified by the concrete improver.
    *
    * The function that takes a gradient, an internal state and an integer t and produces an improvement and a new
    * internal state.
    *
    * The improvement is the vector that is added to the parameter in order to increase the objective variable.
    * t represents the iteration count (how many times the improver has been applied to that parameter so far).
    *
    */
  protected def f[P]:(Vector[+[P,State[P]],S],Int) => Vector[+[P,State[P]],S]

  /**
    * The initialization of the internal state.
    */
  protected def init[P](space:VectorSpace[P,S]):Vector[State[P],S]

  /**
    * Produces a new improver that behaves the same as this, except with a learning rate that is scaled by a factor of
    * k (all the improvements are simply scaled by k).
    */
  def *(k:S):Improver[S] = new Improver[S] {

    private[improver] type State[P] = outer.State[P]

    protected val evaluator = outer.evaluator

    protected def init[P](space:VectorSpace[P,S]):Vector[State[P],S] = outer.init(space)
    protected def f[P]:(Vector[+[P,State[P]],S],Int) => Vector[+[P,State[P]],S] =
      (x:Vector[+[P,State[P]],S],i:Int) => outer.f[P](x,i)*k

  }

  /**
    * Adds this improver to that improver and produces a new improver whose improvements are simply the sum of the
    * improvements produced by this and that.
    */
  def +(that:Improver[S]):Improver[S] = new Improver[S] {



    private[improver] type State[P] = +[outer.State[P],that.State[P]]

    protected val evaluator = outer.evaluator

    protected def init[P](space:VectorSpace[P,S]):Vector[State[P],S] = outer.init(space) | that.init(space)
    protected def f[P]:(Vector[+[P,State[P]],S],Int) => Vector[+[P,State[P]],S] =
      (x:Vector[+[P,State[P]],S],i:Int) => {

        val xp = x.asPair[P,State[P],+[P,State[P]]]

        val grad = xp.left
        val states = xp.right.asPair[outer.State[P],that.State[P],State[P]]

        val l = outer.f(grad|states.left,i).asPair[P,outer.State[P],+[P,outer.State[P]]]
        val r = that.f(grad|states.right,i).asPair[P,that.State[P],+[P,that.State[P]]]

        (l.left + r.left) | (l.right | r.right)
      }

  }

  /**
    * Makes a small adjustment to parameters to achieve all objectives connected to parameters.
    */
  def step(parameter:Parameter[_,S]*): Unit = {
    val grad:Environment = evaluator.gradients(parameter:_*)
    parameter.foreach {p =>
      if (!parameters.contains(p)) {
        parameters(p) = this(p)
      }
      type P = p.space.shape.Type
      val opt = parameters(p).asInstanceOf[OptimizationSession[P,State[P],S]]
      opt.learn(grad.feedback(p).asInstanceOf[Vector[P,S]])
    }
  }

  /**
    * Saves the values of all the parameters along with the internal states of the OptimizationSessions and the
    * iteration counts.
    */
  def save(): Unit = {
    parameters.values.foreach(_.save())
  }

  /**
    * Disconnects all parameters encountered by this improver from the computation graph.
    */
  def disconnect():Unit = {
    parameters.keys.foreach { p =>
      type P = p.space.shape.Type
      val param = p.asInstanceOf[Parameter[P,S]]
      param.sockets.foreach { socket =>
        param -/- socket
      }
    }
  }

  private def apply[P](param:Parameter[P,S]): OptimizationSession[_,_,S] =
    new OptimizationSession[P,State[P],S](f,init(param.space),param)

}

/**
  * Objects of this class are created by Improver in order to keep track of the internal state associated with the
  * improvement of each parameter that it encounters.
  */
class OptimizationSession[P,State,S](f:(Vector[+[P,State],S],Int) => Vector[+[P,State],S],
                                     init:Vector[State,S], p:Parameter[P,S]) {

  private var state = init
  private var i = 1

  import java.nio.file.{Files,Paths}

  if (Files.exists(Paths.get(p.name + "_state"))) {
    state = init.space.open(p.name + "_state")
  }
  if(Files.exists(Paths.get(p.name + "_iteration"))) {
    i = Primitives.deserializeInt(Files.readAllBytes(Paths.get(p.name + "_iteration")).iterator)
  }

  def save(): Unit = {
    p.save()
    if (state.space.shape.nonEmpty) {
      state.save(p.name + "_state")
    }
    Files.write(Paths.get(p.name + "_iteration"),Primitives.serializeInt(i).toArray)
  }

  def learn(gradient:Vector[P,S]): Unit = {
    val r = f(gradient | state,i).asPair[P,State,+[P,State]]
    state = r.right
    p.add(r.left)
    i += 1
  }

}