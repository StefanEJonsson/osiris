// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.pin

import osiris._
import pin.node.merge.PairMerge
import pin.node.split.PairSplit
import osiris.evaluator.Environment
import osiris.pin.node.Node
import vector.space.VectorSpace

trait Socket[I,S] {

  val space:VectorSpace[I,S]

  private var p:Option[Pin[I,S]] = Option.empty

  val node:Node

  def pin:Option[Pin[I,S]] = p

  def feedbackDependencies:Set[Either[Pin[_,_],Pin[_,_]]]

  def evaluateFeedback(environment: Environment): Unit

  def connect(pin:Pin[I,S]):Unit = {
    p = Option(pin)
  }

  def disconnect(): Unit = {
    p = Option.empty
  }

  def split[L,R,P<:Either[L,R] with I]():(Socket[L,S],Socket[R,S]) = {
    val ps = space.asPairSpace[L,R,P]
    val merge = new PairMerge(ps.left,ps.right)
    merge.out ->- this.asInstanceOf[Socket[Either[L,R],S]]
    (merge.left,merge.right)
  }

  def merge[R](that:Socket[R,S]):Socket[Either[I,R],S] = {
    val split = new PairSplit(this.space,that.space)
    split.left ->- this
    split.right ->- that
    split.in
  }

}
