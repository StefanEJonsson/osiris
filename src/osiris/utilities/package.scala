// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris

package object utilities {

  def same[A](x:A,y:A):A =  if(x == y) {x} else {throw new Exception(s"$x and $y were expected to be equal")}

  def absurd[A](n:Nothing):A = throw new Exception(s"Found element of the empty set: $n")

}
