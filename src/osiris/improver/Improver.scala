// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.improver

import osiris._
import osiris.pin.Socket
import osiris.evaluator.Environment
import osiris.pin.variable.Parameter
import osiris.vector.space.VectorSpace
import vector.Vector

trait Improver[S] {

  private val outer = this

  private[improver] type State[P]

  protected val evaluator:osiris.evaluator.Evaluator

  private val parameters = collection.mutable.Map[Parameter[_,S],Optimization[_,_,S]]()

  protected def f[P]:(Vector[+[P,State[P]],S],Int) => Vector[+[P,State[P]],S]
  protected def init[P](space:VectorSpace[P,S]):Vector[State[P],S]


  def *(k:S):Improver[S] = new Improver[S] {

    private[improver] type State[P] = outer.State[P]

    protected val evaluator = outer.evaluator

    protected def init[P](space:VectorSpace[P,S]):Vector[State[P],S] = outer.init(space)
    protected def f[P]:(Vector[+[P,State[P]],S],Int) => Vector[+[P,State[P]],S] =
      (x:Vector[+[P,State[P]],S],i:Int) => outer.f[P](x,i)*k

  }

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

  def step(parameter:Parameter[_,S]*): Unit = {
    val grad:Environment = evaluator.gradients(parameter:_*)
    parameter.foreach {p =>
      if (!parameters.contains(p)) {
        parameters(p) = this(p)
      }
      type P = p.space.shape.Type
      val opt = parameters(p).asInstanceOf[Optimization[P,State[P],S]]
      opt.learn(grad.feedback(p).asInstanceOf[Vector[P,S]])
    }
  }

  def save(): Unit = {
    parameters.values.foreach(_.save())
  }

  def disconnect():Unit = {
    parameters.keys.foreach { p =>
      type P = p.space.shape.Type
      val param = p.asInstanceOf[Parameter[P,S]]
      param.sockets.foreach { socket =>
        param -/- socket
      }
    }
  }

  private def apply[P](param:Parameter[P,S]): Optimization[_,_,S] =
    new Optimization[P,State[P],S](f,init(param.space),param)

}

class Optimization[P,State,S](f:(Vector[+[P,State],S],Int) => Vector[+[P,State],S],
                                         init:Vector[State,S], p:Parameter[P,S]) {

  private var state = init
  private var i = 1

  import java.nio.file.{Files,Paths}

  if (Files.exists(Paths.get(p.name + "_state"))) {
    state = init.space.open(p.name + "_state")
  }
  if(Files.exists(Paths.get(p.name + "_iteration"))) {
    i = utilities.Serialization.Primitives.deserializeInt(Files.readAllBytes(Paths.get(p.name + "_iteration")).iterator)
  }

  def save(): Unit = {
    p.save()
    if (state.space.shape.nonEmpty) {
      state.save(p.name + "_state")
    }
    Files.write(Paths.get(p.name + "_iteration"),utilities.Serialization.Primitives.serializeInt(i).toArray)
  }

  def learn(gradient:Vector[P,S]): Unit = {
    val r = f(gradient | state,i).asPair[P,State,+[P,State]]
    state = r.right
    p.add(r.left)
    i += 1
  }

}