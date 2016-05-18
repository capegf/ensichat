package com.nutomic.ensichat.core.util

import java.util.GregorianCalendar

import com.nutomic.ensichat.core.body.UserInfo
import com.nutomic.ensichat.core.header.ContentHeader
import com.nutomic.ensichat.core.{Message, Address}
import junit.framework.TestCase

class RouteMessageInfoTest extends TestCase {

  private def generateMessage(sender: Address, receiver: Address, seqNum: Int): Message = {
    val header = new ContentHeader(sender, receiver, seqNum, UserInfo.Type, Some(5),
      Some(new GregorianCalendar(2014, 6, 10).getTime))
    new Message(header, new UserInfo("", ""))
  }

}