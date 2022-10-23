// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.morphism

import osiris._
import osiris.utilities.serialization
import osiris.utilities.serialization.v2
import osiris.utilities.serialization.v2.Primitives
import shape.Range

/**
  * Important morphisms associated with Int.
  */
object int {

  def translate(dx:Int,dom:Range):Isomorphism[Int,Int] = new Isomorphism[Int,Int] {

    val domain = dom
    val target = range(domain.start + dx, domain.end + dx)

    override def toString():String = s"translate($dom,$dx)"

    def serialize: Iterable[Byte] =
      Iterable(v2.Morphism.constants.translate) ++
        dom.serialize ++ Primitives.serializeInt(dx)

    def apply(x:Int):Int = x + dx

    lazy val inverse = translate(-dx,target)

  }

  def inv(dom:Range):Isomorphism[Int,Int] = new Isomorphism[Int,Int] {

    val domain = dom
    val target = range(-domain.end,-domain.start)

    override def toString():String = s"inv($dom)"

    def serialize: Iterable[Byte] = Iterable(v2.Morphism.constants.inv) ++ dom.serialize

    def apply(x:Int):Int = -x

    val inverse = this

  }

}
