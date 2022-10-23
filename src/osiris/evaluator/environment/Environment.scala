// SPDX-License-Identifier: GPL-2.1
//Copyright © 2020 Stefán E. Jónsson

package osiris.evaluator.environment

import osiris.evaluator.machine.Instruction
import osiris.pin.Pin
import osiris.vector.space.VectorSpace

trait Environment[Object[_,_]] {

  protected def add[I,S](a:Object[I,S],b:Object[I,S]):Object[I,S]

  protected def zeros[I,S](space:VectorSpace[I,S]):Object[I,S]

  private val values = collection.mutable.Map[Pin[_,_],Object[_,_]]()

  private val feedbacks = collection.mutable.Map[Pin[_,_],Object[_,_]]()

  private val finalFeedbacks = collection.mutable.Set[Pin[_,_]]()

  def run(computation:Instruction):Unit

  def value[I,S](pin:Pin[I,S]):Object[I,S] =
    pin.synchronized {
      while (!values.synchronized{values.isDefinedAt(pin)}) {pin.wait()}
      values.synchronized {values(pin).asInstanceOf[Object[I,S]]}
    }

  def feedback[I,S](pin:Pin[I,S]):Object[I,S] = pin.synchronized {
    while (!finalFeedbacks.synchronized {finalFeedbacks.contains(pin)} ) {pin.wait()}
    feedbacks.synchronized {feedbacks.getOrElse(pin,zeros(pin.space)).asInstanceOf[Object[I,S]]}
  }

  def putValue[I,S](pin:Pin[I,S], value:Object[I,S]): Unit = {
    values.synchronized { values(pin) = value }
    pin.synchronized { pin.notifyAll() }
  }

  def putFeedback[I,S](pin:Pin[I,S],value:Object[I,S]): Unit = feedbacks.synchronized {
    if (feedbacks.contains(pin)) {
      feedbacks(pin) = add[I,S](feedbacks(pin).asInstanceOf[Object[I,S]],value)
    } else {
      feedbacks(pin) = value
    }
  }

  def finalizeFeedback[I,S](pin:Pin[I,S]):Unit = {
    finalFeedbacks.synchronized {finalFeedbacks.add(pin)}
    pin.synchronized {pin.notifyAll()}
  }

  def removeValue[I,S](pin:Pin[I,S]): Unit = values.synchronized {
    values.remove(pin)
  }

  def removeFeedback[I,S](pin:Pin[I,S]): Unit = {
    feedbacks.synchronized { feedbacks.remove(pin) }
    finalFeedbacks.synchronized { finalFeedbacks.remove(pin) }
  }

}
