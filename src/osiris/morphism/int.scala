// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.morphism

import osiris._
import shape.Range

object int {

  def translate(dx:Int,dom:Range):Isomorphism[Int,Int] = new Isomorphism[Int,Int] {

    val domain = dom
    val target = range(domain.start + dx, domain.end + dx)

    override def toString():String = s"translate($dom,$dx)"

    def serialize: Iterable[Byte] =
      Iterable(utilities.Serialization.Morphism.translate) ++
        dom.serialize ++ utilities.Serialization.Primitives.serializeInt(dx)

    def apply(x:Int):Int = x + dx

    lazy val inverse = translate(-dx,target)

  }

  def inv(dom:Range):Isomorphism[Int,Int] = new Isomorphism[Int,Int] {

    val domain = dom
    val target = range(-domain.end,-domain.start)

    override def toString():String = s"inv($dom)"

    def serialize: Iterable[Byte] = Iterable(utilities.Serialization.Morphism.inv) ++ dom.serialize

    def apply(x:Int):Int = -x

    val inverse = this

  }

}
