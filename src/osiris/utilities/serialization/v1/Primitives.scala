package osiris.utilities.serialization.v1

import java.nio.ByteOrder

object Primitives {

  private var byteOrder = ByteOrder.LITTLE_ENDIAN

  def bigEndian(): Unit = {
    byteOrder = ByteOrder.BIG_ENDIAN
  }

  def littleEndian(): Unit = {
    byteOrder = ByteOrder.LITTLE_ENDIAN
  }

  def deserializeInt(bytes:Iterator[Byte]):Int = {
    val arr = new Array[Byte](4)
    for (i <- 0 until 4) {arr(i) = bytes.next()}
    java.nio.ByteBuffer.wrap(arr).order(byteOrder).getInt()
  }

  def deserializeFloat(bytes:Iterator[Byte]):Float =  {
    val arr = new Array[Byte](4)
    for (i <- 0 until 4) {arr(i) = bytes.next()}
    java.nio.ByteBuffer.wrap(arr).order(byteOrder).getFloat()
  }

  def deserializeDouble(bytes:Iterator[Byte]):Double =  {
    val arr = new Array[Byte](8)
    for (i <- 0 until 8) {arr(i) = bytes.next()}
    java.nio.ByteBuffer.wrap(arr).order(byteOrder).getDouble()
  }

}
