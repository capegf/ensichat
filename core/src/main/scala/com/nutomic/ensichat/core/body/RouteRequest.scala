package com.nutomic.ensichat.core.body

import java.nio.ByteBuffer

import com.nutomic.ensichat.core.Address
import com.nutomic.ensichat.core.util.BufferUtils

object RouteRequest {

  val Type = 2

  /**
   * Constructs [[RouteRequest]] instance from byte array.
   */
  def read(array: Array[Byte]): RouteRequest = {
    val b = ByteBuffer.wrap(array)
    val requested  = new Address(BufferUtils.getByteArray(b, Address.Length))
    val origSeqNum = BufferUtils.getUnsignedShort(b)
    val targSeqNum = BufferUtils.getUnsignedShort(b)
    val originMetric = BufferUtils.getUnsignedShort(b)
    new RouteRequest(requested, origSeqNum, targSeqNum, originMetric)
  }

}

case class RouteRequest(requested: Address, origSeqNum: Int, targSeqNum: Int, originMetric: Int) extends MessageBody {

  override def protocolType = RouteRequest.Type

  override def contentType = -1

  override def write: Array[Byte] = {
    val b = ByteBuffer.allocate(length)
    b.put(requested.bytes)
    BufferUtils.putUnsignedShort(b, origSeqNum)
    BufferUtils.putUnsignedShort(b, targSeqNum)
    BufferUtils.putUnsignedShort(b, originMetric)
    b.array()
  }

  override def length = 6 + Address.Length

}
