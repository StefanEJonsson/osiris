// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.evaluator

import osiris.pin._
import osiris.vector.{Empty, Matrix, Pair, Sequential, Single, Vector}

/**
  * Contains a mapping between pins and values (and also a mapping between pins and gradients).
  *
  * It is used by Evaluators to keep track of intermediate results when evaluating a computation graph. Because every
  * evaluator creates its own environment, it is possible for multiple evaluators to work simultaneously on the same
  * graph.
  *
  */
class Environment {

  private val values = collection.mutable.Map[Pin[_,_],Vector[_,_]]()

  private val feedbacks = collection.mutable.Map[Pin[_,_],Vector[_,_]]()

  def apply[I,S](pin:Pin[I,S]):Vector[I,S] = synchronized {values(pin).asInstanceOf[Vector[I,S]]}

  def apply[S](pin:EmptyPin[S]):Empty[S] = synchronized {values(pin).asInstanceOf[Empty[S]]}

  def apply[S](pin:SinglePin[S]):Single[S] = synchronized {values(pin).asInstanceOf[Single[S]]}

  def apply[S](pin:SequentialPin[S]):Sequential[S] = synchronized {values(pin).asInstanceOf[Sequential[S]]}

  def apply[L,R,S](pin:PairPin[L,R,S]):Pair[L,R,S] = synchronized {values(pin).asInstanceOf[Pair[L,R,S]]}

  def apply[I,J,S](pin:MatrixPin[I,J,S]):Matrix[I,J,S] = synchronized {values(pin).asInstanceOf[Matrix[I,J,S]]}

  def feedback[I,S](pin:Pin[I,S]):Vector[I,S] =
    synchronized {feedbacks.getOrElse(pin,pin.space.zeros).asInstanceOf[Vector[I,S]]}

  def put[I,S](pin:Pin[I,S],value:Vector[I,S]): Unit = synchronized {
    values(pin) = value
  }

  def putFeedback[I,S](pin:Pin[I,S],value:Vector[I,S]): Unit = synchronized {
    if (feedbacks.contains(pin)) {
      feedbacks(pin) = feedback(pin) + value
    } else {
      feedbacks(pin) = value
    }
  }

  def remove[S](pin:Pin[_,S]): Unit = synchronized {
    values.remove(pin)
  }

  def removeFeedback[S](pin:Pin[_,S]): Unit = synchronized {
    feedbacks.remove(pin)
  }

}
