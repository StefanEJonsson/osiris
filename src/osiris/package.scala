// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package object osiris {

  type +[X,Y] = Either[X,Y]
  type *[X,Y] = Tuple2[X,Y]

  val I = shape.Single
  val O = shape.Empty

  /**
    * The shape with indices {start, start+1, ..., end}
    */
  def range(start:Int,end:Int):shape.Range = new shape.Range(start,end)

  /**
    * The shape with indices {0, 1, ..., n-1}
    */
  def until(n:Int):shape.Range = range(0,n-1)


}
