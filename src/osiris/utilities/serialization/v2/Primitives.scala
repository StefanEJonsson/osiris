package osiris.utilities.serialization.v2

import java.nio.ByteOrder

object Primitives {

  def serializeInt(i:Int):Iterable[Byte] =
    java.nio.ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(i).array()
  def deserializeInt(bytes:Iterator[Byte]):Int = {
    val arr = new Array[Byte](4)
    for (i <- 0 until 4) {arr(i) = bytes.next()}
    java.nio.ByteBuffer.wrap(arr).order(ByteOrder.LITTLE_ENDIAN).getInt()
  }

  def serializeFloat(f:Float):Iterable[Byte] =
    java.nio.ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putFloat(f).array()
  def deserializeFloat(bytes:Iterator[Byte]):Float =  {
    val arr = new Array[Byte](4)
    for (i <- 0 until 4) {arr(i) = bytes.next()}
    java.nio.ByteBuffer.wrap(arr).order(ByteOrder.LITTLE_ENDIAN).getFloat()
  }

  def serializeDouble(d:Double):Iterable[Byte] =
    java.nio.ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN).putDouble(d).array()
  def deserializeDouble(bytes:Iterator[Byte]):Double =  {
    val arr = new Array[Byte](8)
    for (i <- 0 until 8) {arr(i) = bytes.next()}
    java.nio.ByteBuffer.wrap(arr).order(ByteOrder.LITTLE_ENDIAN).getDouble()
  }

}
