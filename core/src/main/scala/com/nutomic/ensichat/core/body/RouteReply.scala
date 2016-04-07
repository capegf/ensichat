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
    val targMetric = BufferUtils.getUnsignedShort(b)
    new RouteReply(targMetric)
  }

}

case class RouteReply(targMetric: Int) extends MessageBody {

  override def protocolType = RouteReply.Type

  override def contentType = -1

  override def write: Array[Byte] = {
    val b = ByteBuffer.allocate(length)
    BufferUtils.putUnsignedShort(b, targMetric)
    b.array()
  }

  override def length = 2

}
