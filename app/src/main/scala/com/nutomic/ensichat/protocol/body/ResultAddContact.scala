package com.nutomic.ensichat.protocol.body

import java.nio.ByteBuffer

import com.nutomic.ensichat.util.BufferUtils

object ResultAddContact {

  val Type = 2

  /**
   * Constructs [[ResultAddContact]] instance from byte array.
   */
  def read(array: Array[Byte]): ResultAddContact = {
    val b = ByteBuffer.wrap(array)
    val first = BufferUtils.getUnsignedByte(b)
    val accepted = (first & 0x80) != 0
    new ResultAddContact(accepted)
  }

}

/**
 * Contains the result of a [[RequestAddContact]] message.
 */
case class ResultAddContact(accepted: Boolean) extends MessageBody {

  override def protocolType = -1

  override def contentType = ResultAddContact.Type

  override def write: Array[Byte] = {
    val b = ByteBuffer.allocate(length)
    BufferUtils.putUnsignedByte(b, if (accepted) 0x80 else 0)
    (0 to 1).foreach(_ => BufferUtils.putUnsignedByte(b, 0))
    b.array()
  }

  override def length = 4

}
