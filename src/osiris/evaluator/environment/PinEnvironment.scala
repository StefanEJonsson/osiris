// SPDX-License-Identifier: GPL-2.1
//Copyright © 2020 Stefán E. Jónsson

package osiris.evaluator.environment

import osiris.evaluator.machine.{Instruction, FeedbackInstruction, FinalizeFeedback, NodeInstruction}
import osiris.{I, O, Objective}
import osiris.pin.{Pin, Socket}
import osiris.pin.node._
import osiris.pin.node.cast._
import osiris.pin.node.merge.{Merge, PairMerge, RowMerge}
import osiris.pin.node.replace.{Replace, ReplaceCol, ReplaceRow}
import osiris.pin.node.split.{PairSplit, RowSplit, Split}
import osiris.vector.space.VectorSpace

class PinEnvironment extends Environment[Pin] {

  private val feedbacks = collection.mutable.Map[Pin[_,_],Pin[_,_]]()

  protected def add[I,S](a:Pin[I,S],b:Pin[I,S]):Pin[I,S] = a + b

  protected def zeros[I,S](space:VectorSpace[I,S]):Pin[I,S] = space.constant.zeros

  def run(comp:Instruction): Unit = comp match {
      case FinalizeFeedback(pin) => finalizeFeedback(pin)
      case NodeInstruction(node) => {}
      case FeedbackInstruction(socket) => socket match {
        case (o:Objective[_]) => {
          type S = o.space.scalarSpace.Type
          putFeedback(o.pin.get,(I --> o.space.scalarSpace).constant(_ => o.strength.asInstanceOf[S]))
        }
        case _ => {
          socket.node match {
            case (f:BilinearNode[_,_,_,_]) => {
              if (socket == f.left) {
                val node = new BilinearNode(f.f.leftFeedback)
                feedback(f.out) ->- node.left
                f.right.pin.get ->- node.right
                putFeedback(f.left.pin.get,node.out)
              } else {
                val node = new BilinearNode(f.f.rightFeedback)
                feedback(f.out) ->- node.left
                f.left.pin.get ->- node.right
                putFeedback(f.right.pin.get,node.out)
              }
            }
            case (f:LinearNode[_,_,_]) => {
              val node = new LinearNode(f.f.linearFeedback)
              feedback(f.out) ->- node.in
              putFeedback(f.in.pin.get,node.out)
            }
            case (f:FunctionNode[_,_,_]) => {
              val node = new FunctionNode(f.f.feedback)
              type S = node.out.space.scalarSpace.Type
              val in = f.in.pin.get.asInstanceOf[Pin[_,S]] | feedback(f.out)
              type X = in.space.shape.Type
              in.asInstanceOf[Pin[X,S]] ->- node.in.asInstanceOf[Socket[X,S]]
              putFeedback(f.in.pin.get,node.out)
            }
            case (tick:Tick[_,_]) => {
              putFeedback(tick.in.pin.get,feedback(tick.out)*tick.discountFactor)
            }
            case (adv:Adversarial[_,_]) => {
              putFeedback(adv.in.pin.get,-adv.out)
            }
            case (rep:Replace[_,_]) => {
              type S = rep.space.scalarSpace.Type
              type I = rep.space.shape.Type
              putFeedback(
                rep.in.pin.get,
                feedback(rep.out).asInstanceOf[Pin[I,S]].replace(
                  rep.i.asInstanceOf[I],
                  rep.replacement.space.constant.zeros
                )
              )
              putFeedback[I,S](
                rep.in.pin.get,
                feedback(rep.out).asInstanceOf[Pin[I,S]](rep.i.asInstanceOf[I]).asInstanceOf[Pin[I,S]]
              )
            }
            case (rep:ReplaceRow[_,_,_]) => {
              val space = rep.space.asMatrixSpace
              type S = space.scalarSpace.Type
              type I = space.outer.shape.Type
              type J = space.inner.shape.Type
              putFeedback[(I,J),S](
                rep.in.pin.get.asInstanceOf[Pin[(I,J),S]],
                feedback(rep.out).asInstanceOf[Pin[(I,J),S]].asMatrix.replaceRow(
                  rep.i.asInstanceOf[I],
                  rep.replacement.space.constant.zeros
                ).asInstanceOf[Pin[(I,J),S]]
              )
              putFeedback[I,S](
                rep.replacement.pin.get.asInstanceOf[Pin[I,S]],
                feedback(rep.out).asInstanceOf[Pin[(I,J),S]].asMatrix.row(rep.i.asInstanceOf[I])
              )
            }
            case (rep:ReplaceCol[_,_,_]) => {
              val space = rep.space.asMatrixSpace
              type S = space.scalarSpace.Type
              type I = space.outer.shape.Type
              type J = space.inner.shape.Type
              putFeedback[(I,J),S](
                rep.in.pin.get.asInstanceOf[Pin[(I,J),S]],
                feedback(rep.out).asInstanceOf[Pin[(I,J),S]].asMatrix.replaceCol(
                  rep.j.asInstanceOf[I],
                  rep.replacement.space.constant.zeros
                ).asInstanceOf[Pin[(I,J),S]]
              )
              putFeedback[J,S](
                rep.replacement.pin.get.asInstanceOf[Pin[J,S]],
                feedback(rep.out).asInstanceOf[Pin[(I,J),S]].asMatrix.col(rep.j.asInstanceOf[I])
              )
            }
            case (split:Split[_,_]) => {
              val space = split.in.space
              type I = space.shape.Type
              putFeedback(
                split.in.pin.get,
                space((i:I) => feedback(split(i)))
              )
            }
            case (split:PairSplit[_,_,_]) => {
              putFeedback(
                split.in.pin.get,
                feedback(split.left) | feedback(split.right)
              )
            }
            case (split:RowSplit[_,_,_]) => {
              type I = split.in.space.outer.shape.Type
              type J = split.in.space.inner.shape.Type
              type S = split.in.space.scalarSpace.Type
              val space = split.in.space.asMatrixSpace[I,J,(I,J)]
              putFeedback(
                split.in.pin.get,
                space.rows((i:I) => feedback(split(i)).asInstanceOf[Pin[J,S]])
              )
            }
            case (merge:Merge[_,_]) => {
              type I = merge.out.space.shape.Type
              val space:VectorSpace[I,_] = merge.out.space
              for (i <- space.shape) {
                putFeedback(
                  merge.in(i).pin.get,
                  feedback(merge.out)(i)
                )
              }
            }
            case (merge:PairMerge[_,_,_]) => {
              val space = merge.out.space.asPairSpace
              type L = space.left.shape.Type
              type R = space.right.shape.Type
              type S = space.scalarSpace.Type
              putFeedback[L,S](
                merge.left.pin.get.asInstanceOf[Pin[L,S]],
                feedback(merge.out).asPair.left.asInstanceOf[Pin[L,S]]
              )
              putFeedback[R,S](
                merge.right.pin.get.asInstanceOf[Pin[R,S]],
                feedback(merge.out).asPair.right.asInstanceOf[Pin[R,S]]
              )
            }
            case (merge:RowMerge[_,_,_]) => {
              type I = merge.out.space.outer.shape.Type
              type J = merge.out.space.inner.shape.Type
              val space = merge.out.space.asMatrixSpace[I,J,(I,J)]
              for (i <- space.outer.shape) {
                putFeedback(merge.in(i).pin.get,feedback(merge.out).asMatrix[I,J,(I,J)].row(i))
              }
            }
            case (cast:EmptyCast[_]) => {
              val s = cast.in.space.scalarSpace
              putFeedback(cast.in.pin.get,(O --> s).constant.zeros)
            }
            case (cast:SingleCast[_]) => {
              putFeedback(cast.in.pin.get,feedback(cast.out))
            }
            case (cast:SequentialCast[_]) => {
              putFeedback(cast.in.pin.get,feedback(cast.out))
            }
            case (cast:PairCast[_,_,_]) => {
              putFeedback(cast.in.pin.get,feedback(cast.out))
            }
            case (cast:MatrixCast[_,_,_]) => {
              putFeedback(cast.in.pin.get,feedback(cast.out))
            }
            case (cast:ScalarCast[_,_,_]) => {
              type I = cast.shape.Type
              type S1 = cast.from.Type
              type S2 = cast.to.Type
              val ccast = cast.asInstanceOf[ScalarCast[I,S1,S2]]
              val inv = new ScalarCast[I,S2,S1](cast.shape,cast.to,cast.from,ccast.inverseCast,ccast.cast)
              feedback(cast.out).asInstanceOf[Pin[I,S2]] ->- inv.in
              putFeedback(cast.in.pin.get,inv.out)
            }
          }
        }
      }
    }


}
