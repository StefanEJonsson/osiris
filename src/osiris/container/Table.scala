// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.container

import osiris._
import morphism._
import container.companion._

trait Table[I,J,S] extends Container[(I,J),S] {

  /* ---------------------------------------------------------------------------------------------------------------- */

  val outer:ContainerCompanion[I,S]
  val inner:ContainerCompanion[J,S]

  lazy val space:TableCompanion[I,J,S] = outer * inner

  override def toString():String = {
    val k = 22 //TODO make configurable
    def s(n:Int):String = " " * n
    def shorten(st:S):String = {
      val ss = st.toString
      if(ss.length > k-2) {ss.substring(0,k-2)}
      else {ss + s(k-2-ss.length)}
    }
    var sb = s(k)
    for(j <- inner.shape) {
      val js = j.toString
      sb = sb + js + s(k-js.length)
    }
    sb = sb + "\n"
    for(i <- outer.shape) {
      val is = i.toString
      sb = sb + is + s(k-is.length)
      for(j <- inner.shape) {
        sb = sb + shorten(this((i,j))) + s(2)
      }
      sb = sb + "\n"
    }
    sb
  }

  /* ---------------------------------------------------------------------------------------------------------------- */

  def transpose:Table[J,I,S] =
    this.reIndex[(J,I)](product.commute(inner.shape,outer.shape)).asTable

  def row(i:I):Container[J,S] = this.reIndex[J]( product.rightPairedWith(outer.shape,inner.shape,i) )

  def col(j:J):Container[I,S] = this.reIndex[I]( product.leftPairedWith(outer.shape,inner.shape,j) )

  /* ---------------------------------------------------------------------------------------------------------------- */

  def rowMap[J2](newInner:ContainerCompanion[J2,S],f:Container[J,S] => Container[J2,S]):Table[I,J2,S] =
    (outer * newInner).rows( i => f(row(i)) )

  def colMap[I2](newOuter:ContainerCompanion[I2,S],f:Container[I,S] => Container[I2,S]):Table[I2,J,S] =
    (newOuter * inner).cols( j => f(col(j)) )


  def rowMap(f:Container[J,S] => S):Container[I,S] = outer( i => f(row(i)) )

  def colMap(f:Container[I,S] => S):Container[J,S] = inner( j => f(col(j)) )

  /* ---------------------------------------------------------------------------------------------------------------- */

  def rowWise[JThat,JRes](target:ContainerCompanion[JRes,S],
                          op:(Container[J,S],Container[JThat,S]) => Container[JRes,S]
                         )(that:Table[I,JThat,S]):Table[I,JRes,S] =
    (outer*target).rows( i => op(this.row(i),that.row(i)) )

  def colWise[IThat,IRes](target:ContainerCompanion[IRes,S],
                          op:(Container[I,S],Container[IThat,S]) => Container[IRes,S]
                         )(that:Table[IThat,J,S]):Table[IRes,J,S] =
    (target*inner).cols( j => op(this.col(j),that.col(j)) )

  def rowWise[JThat](op:(Container[J,S],Container[JThat,S]) => S)(that:Table[I,JThat,S]):Container[I,S] =
    outer(i => op(this.row(i),that.row(i)))

  def colWise[IThat](op:(Container[I,S],Container[IThat,S]) => S)(that:Table[IThat,J,S]):Container[J,S] =
    inner(j => op(this.col(j),that.col(j)))

}

class UnCurry[I,J,S](original:Container[I,Container[J,S]],val inner:ContainerCompanion[J,S])
  extends Table[I,J,S] {

  val outer = original.space.shape-->[S]()

  def apply(t:(I,J)):S = original(t._1)(t._2)

}