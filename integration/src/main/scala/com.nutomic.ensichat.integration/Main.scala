package com.nutomic.ensichat.integration

import java.io.File
import java.util.concurrent.{LinkedBlockingDeque, CountDownLatch, TimeUnit}

import com.nutomic.ensichat.core.{Message, Crypto}
import com.nutomic.ensichat.core.body.{RouteRequest, Text}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Try

/**
 * Creates some local nodes, connects them and sends messages between them.
 *
 * If the test runs slow or fails, changing [[Crypto.PublicKeySize]] to 512 should help.
 */
object Main extends App {

  val nodes = createMesh()
  System.out.println("\n\nAll nodes connected!\n\n")

  sendMessages(nodes)
  System.out.println("\n\nAll messages sent!\n\n")

  private def createMesh(): Seq[LocalNode] = {
    val nodes = Await.result(Future.sequence(0.to(2).map(createNode)), Duration.Inf)
    sys.addShutdownHook(nodes.foreach(_.stop()))

    connectNodes(nodes(0), nodes(1))
    connectNodes(nodes(1), nodes(2))
    nodes.foreach(n => System.out.println(s"Node ${n.index} has address ${n.crypto.localAddress}"))

    nodes
  }

  private def createNode(index: Int): Future[LocalNode] = {
    val configFolder = new File(s"build/node$index/")
    assert(!configFolder.exists(), s"stale config exists in $configFolder")
    Future(new LocalNode(index, configFolder))
  }

  private def connectNodes(first: LocalNode, second: LocalNode): Unit = {
    first.connectionHandler.connect(s"localhost:${second.port}")

    first.eventQueue.toStream.find(_._1 == LocalNode.EventType.ConnectionsChanged)
    second.eventQueue.toStream.find(_._1 == LocalNode.EventType.ConnectionsChanged)

    val firstAddress      = first.crypto.localAddress
    val secondAddress     = second.crypto.localAddress
    val firstConnections  = first.connectionHandler.connections()
    val secondConnections = second.connectionHandler.connections()

    assert(firstConnections.contains(secondAddress),
      s"${first.index} is not connected to ${second.index}")
    assert(secondConnections.contains(firstAddress),
      s"${second.index} is not connected to ${second.index}")

    System.out.println(s"${first.index} and ${second.index} connected")
  }

  private def sendMessages(nodes: Seq[LocalNode]): Unit = {
    sendMessage(nodes(0), nodes(1))
    sendMessage(nodes(0), nodes(2))
  }


  private def sendMessage(from: LocalNode, to: LocalNode): Unit = {
    addKey(to.crypto, from.crypto)
    addKey(from.crypto, to.crypto)

    System.out.println(s"sendMessage(${from.index}, ${to.index})")
    val text = s"${from.index} to ${to.index}"
    from.connectionHandler.sendTo(to.crypto.localAddress, new Text(text))

    val latch = new CountDownLatch(1)
    Future {
      val exists =
        to.eventQueue.toStream.exists { event =>
          if (event._1 != LocalNode.EventType.MessageReceived)
            false
          else {
            event._2.get.body match {
              case t: Text => t.text == text
              case _ => false
            }
          }
        }
      assert(exists, s"message from ${from.index} did not arrive at ${to.index}")
      latch.countDown()
    }
    assert(latch.await(500, TimeUnit.MILLISECONDS))
  }

  private def addKey(addTo: Crypto, addFrom: Crypto): Unit = {
    if (Try(addTo.getPublicKey(addFrom.localAddress)).isFailure)
      addTo.addPublicKey(addFrom.localAddress, addFrom.getLocalPublicKey)

  }

}