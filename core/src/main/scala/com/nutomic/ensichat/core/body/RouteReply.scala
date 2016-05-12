package com.nutomic.ensichat.core.body

import java.nio.ByteBuffer

import com.nutomic.ensichat.core.Address
import com.nutomic.ensichat.core.util.BufferUtils

object RouteReply {

  val Type = 3

  /**
   * Constructs [[RouteReply]] instance from byte array.
   */
  def read(array: Array[Byte]): RouteReply = {
    val b = ByteBuffer.wrap(array)
    val targAddress  = new Address(BufferUtils.getByteArray(b, Address.Length))
    val targSeqNum = BufferUtils.getUnsignedShort(b)
    val targMetric = BufferUtils.getUnsignedShort(b)
    new RouteReply(targAddress, targSeqNum, targMetric)
  }

}

case class RouteReply(targAddress: Address, targSeqNum: Int, targMetric: Int) extends MessageBody {

  override def protocolType = RouteReply.Type

  override def contentType = -1

  override def write: Array[Byte] = {
    val b = ByteBuffer.allocate(length)
    b.put(targAddress.bytes)
    BufferUtils.putUnsignedShort(b, targSeqNum)
    BufferUtils.putUnsignedShort(b, targMetric)
    b.array()
  }

  override def length = 4 + Address.Length

}
