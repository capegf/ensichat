package com.nutomic.ensichat.protocol.messages

import java.nio.ByteBuffer

import com.nutomic.ensichat.protocol.{Address, BufferUtils}

object AbstractHeader {

  val DefaultHopLimit = 20

  val Version = 0

  private[messages] val Length = 10 + 2 * Address.Length

}

/**
 * Contains the header fields and functionality that are used both in [[MessageHeader]] and
 * [[ContentHeader]].
 */
trait AbstractHeader {

  def protocolType: Int
  def hopLimit: Int
  def hopCount: Int
  def origin: Address
  def target: Address
  def seqNum: Int

  /**
   * Writes the header to byte array.
   */
  def write(contentLength: Int): Array[Byte] = {
    val b = ByteBuffer.allocate(AbstractHeader.Length)

    BufferUtils.putUnsignedByte(b, AbstractHeader.Version)
    BufferUtils.putUnsignedByte(b, protocolType)
    BufferUtils.putUnsignedByte(b, hopLimit)
    BufferUtils.putUnsignedByte(b, hopCount)

    BufferUtils.putUnsignedInt(b, length + contentLength)
    b.put(origin.bytes)
    b.put(target.bytes)

    BufferUtils.putUnsignedShort(b, seqNum)

    b.array()
  }

  /**
   * Returns true if this object is an instance of [[ContentHeader]].
   */
  def isContentMessage = protocolType == ContentHeader.ContentMessageType

  def length: Int

  override def equals(a: Any): Boolean = a match {
    case o: AbstractHeader =>
      protocolType  == o.protocolType &&
        hopLimit    == o.hopLimit &&
        hopCount    == o.hopCount &&
        origin      == o.origin &&
        target      == o.target &&
        seqNum      == o.seqNum
    case _ => false
  }

}