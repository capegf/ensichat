package com.nutomic.ensichat.core.body

import junit.framework.TestCase
import org.junit.Assert._

class RouteRequestTest extends TestCase {

  def testWriteRead(): Unit = {
    val rreq = new RouteRequest(60000, 60001, 60002)
    val bytes = rreq.write
    val parsed = RouteRequest.read(bytes)
    assertEquals(rreq, parsed)
  }

}
