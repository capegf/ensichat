package com.nutomic.ensichat.core.body

import java.nio.ByteBuffer

import com.nutomic.ensichat.core.util.BufferUtils

object RouteReply {

  val Type = 2

  /**
   * Constructs [[RouteReply]] instance from byte array.
   */
  def read(array: Array[Byte]): RouteReply = {
    val b = ByteBuffer.wrap(array)
    val targSeqNum = BufferUtils.getUnsignedShort(b)
    val targMetric = BufferUtils.getUnsignedShort(b)
    new RouteReply(targSeqNum, targMetric)
  }

}

case class RouteReply(targSeqNum: Int, targMetric: Int) extends MessageBody {

  override def protocolType = RouteReply.Type

  override def contentType = -1

  override def write: Array[Byte] = {
    val b = ByteBuffer.allocate(length)
    BufferUtils.putUnsignedShort(b, targSeqNum)
    BufferUtils.putUnsignedShort(b, targMetric)
    b.array()
  }

  override def length = 2

}
