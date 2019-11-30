// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.utilities

object Serialization {

  val version:Byte = 1


  object Primitives {

    def serializeInt(i:Int):Iterable[Byte] = java.nio.ByteBuffer.allocate(4).putInt(i).array()
    def deserializeInt(bytes:Iterator[Byte]):Int = {
      val arr = new Array[Byte](4)
      for (i <- 0 until 4) {arr(i) = bytes.next()}
      java.nio.ByteBuffer.wrap(arr).getInt()
    }

    def serializeFloat(f:Float):Iterable[Byte] = java.nio.ByteBuffer.allocate(4).putFloat(f).array()
    def deserializeFloat(bytes:Iterator[Byte]):Float =  {
      val arr = new Array[Byte](4)
      for (i <- 0 until 4) {arr(i) = bytes.next()}
      java.nio.ByteBuffer.wrap(arr).getFloat()
    }

    def serializeDouble(d:Double):Iterable[Byte] = java.nio.ByteBuffer.allocate(8).putDouble(d).array()
    def deserializeDouble(bytes:Iterator[Byte]):Double =  {
      val arr = new Array[Byte](8)
      for (i <- 0 until 8) {arr(i) = bytes.next()}
      java.nio.ByteBuffer.wrap(arr).getDouble()
    }

  }

  object Representation {

    val elems:Byte = 1
    val reindex:Byte = 2

  }

  object Shape {

    val sum:Byte= 1
    val product:Byte = 2

    val empty:Byte = 3
    val single:Byte = 4
    val range:Byte = 5

    //val power:Byte = 6
    //val function:Byte = 7

  }

  object Index {

    val left :Byte = 1
    val right:Byte = 2

  }

  object Morphism {

    val id:Byte = 1
    val unit:Byte = 2
    val absurd:Byte = 3
    val constant:Byte = 4
    val table:Byte = 5

    val composition:Byte = 10

    val lmap:Byte = 15
    val rmap:Byte = 16
    val bimap:Byte = 17

    val sum:Byte = 20
    val product:Byte = 21

    val equal:Byte = 30

    val and:Byte = 35
    val or:Byte = 36
    val xor:Byte = 37

    val translate:Byte = 40
    val inv:Byte = 41

    val commute:Byte = 50

    val leftAssoc:Byte = 51
    val rightAssoc:Byte = 52

    val putLeft:Byte = 53
    val putRight:Byte = 54
    val getLeft:Byte = 55
    val getRight:Byte = 56

    val distrLeft:Byte = 57
    val distrRight:Byte = 58
    val extractLeft:Byte = 59
    val extractRight:Byte = 60

    val join:Byte = 70
    val left:Byte = 71
    val right:Byte = 72

    val spookLeft:Byte = 80
    val spookRight:Byte = 81
    val bustLeft:Byte = 82
    val bustRight:Byte = 83

    val first:Byte = 84
    val second:Byte = 85

    val firstWith:Byte = 86
    val secondWith:Byte = 87

    val copy:Byte = 88

  }


}
