// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.bilinear

import osiris._
import osiris.vector._
import osiris.vector.space.{SequentialSpace, VectorSpace}

import scala.reflect.ClassTag

class Convolution[S : ClassTag](val left:SequentialSpace[S],val right:SequentialSpace[S],
                                           val target:SequentialSpace[S])
  extends BilinearFunction[Int,Int,Int,S] {


  override def apply(x:Vector[Either[Int,Int],S]):Vector[Int,S] = {
    val xp = x.asPair[Int,Int,+[Int,Int]]
    val res = (for (i <- target.shape) yield {
      val minJ = Math.max(left.shape.start,i - right.shape.end)   //greatest lower bound
      val maxJ = Math.min(left.shape.end,i - right.shape.start)   //smallest upper bound
      (minJ to maxJ).map { j =>
        scalarSpace.*(xp.left(j),xp.right(i-j))                                //sum of all l(k)*r(j) such that k and j are within their bounds and k+j=i
      }.fold(scalarSpace.zero)(scalarSpace.+(_,_))
    }).toVector
    new SimpleSequential[S](target,res)
  }

  def apply[II,LL,RR](iiSpace:VectorSpace[II,S],llSpace:VectorSpace[LL,S],rrSpace:VectorSpace[RR,S],
                      l:Matrix[Int,LL,S],r:Matrix[Int,RR,S],
                      op:(Vector[LL,S],Vector[RR,S]) => Vector[II,S]):Matrix[Int,II,S] = {

    (target*iiSpace).rows((i:Int) => {
      val minJ = Math.max(left.shape.start,i - right.shape.end + 1) //greatest lower bound
      val maxJ = Math.min(left.shape.end-1,i - right.shape.start)   //smallest upper bound
      (minJ to maxJ).map { j =>
        op(l.row(j),r.row(i-j))                                     //sum of all l(k)*r(j) such that k and j are within their bounds and k+j=i
      }.fold(iiSpace.zeros)(_+_)
    })
  }

  lazy val leftFeedback =
    new Convolution(target,-right.shape --> scalarSpace,left).permuteRight(morphism.int.inv(left.shape))

  lazy val rightFeedback =
    new Convolution(target,-left.shape --> scalarSpace,right).permuteRight(morphism.int.inv(right.shape))

}
