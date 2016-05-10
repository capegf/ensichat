package com.nutomic.ensichat.core.body

import java.nio.ByteBuffer

import com.nutomic.ensichat.core.Address
import com.nutomic.ensichat.core.util.BufferUtils

object RouteError {

  val Type = 3

  /**
   * Constructs [[RouteError]] instance from byte array.
   */
  def read(array: Array[Byte]): RouteError = {
    val b = ByteBuffer.wrap(array)
    val packetSource = new Address(BufferUtils.getByteArray(b, Address.Length))
    val address = new Address(BufferUtils.getByteArray(b, Address.Length))
    val seqNum = BufferUtils.getUnsignedShort(b)
    new RouteError(packetSource, address, seqNum)
  }

}

case class RouteError(packetSource: Address, address: Address, seqNum: Int) extends MessageBody {

  override def protocolType = RouteReply.Type

  override def contentType = -1

  override def write: Array[Byte] = {
    val b = ByteBuffer.allocate(length)
    b.put(packetSource.bytes)
    b.put(address.bytes)
    BufferUtils.putUnsignedShort(b, seqNum)
    b.array()
  }

  override def length = Address.Length * 2 + 2

}