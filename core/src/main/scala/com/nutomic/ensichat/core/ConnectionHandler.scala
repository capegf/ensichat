package com.nutomic.ensichat.core

import java.security.InvalidKeyException
import java.util.Date

import com.nutomic.ensichat.core.body._
import com.nutomic.ensichat.core.header.{MessageHeader, ContentHeader}
import com.nutomic.ensichat.core.interfaces._
import com.nutomic.ensichat.core.internet.InternetInterface
import com.nutomic.ensichat.core.util.{LocalRoutesInfo, Database, FutureHelper, RouteMessageInfo}
import com.typesafe.scalalogging.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * High-level handling of all message transfers and callbacks.
 *
 * @param maxInternetConnections Maximum number of concurrent connections that should be opened by
 *                               [[InternetInterface]].
 */
final class ConnectionHandler(settings: SettingsInterface, database: Database,
                              callbacks: CallbackInterface, crypto: Crypto,
                              maxInternetConnections: Int,
                              port: Int = InternetInterface.DefaultPort) {

  private val logger = Logger(this.getClass)

  private var transmissionInterfaces = Set[TransmissionInterface]()

  private lazy val seqNumGenerator = new SeqNumGenerator(settings)

  private val localRoutesInfo = new LocalRoutesInfo(connections)

  private val routeMessageInfo = new RouteMessageInfo()

  private lazy val router = new Router(localRoutesInfo, sendVia, noRouteFound)

  private var missingRouteMessages = Set[Message]()

  /**
   * Holds all known users.
   *
   * This is for user names that were received during runtime, and is not persistent.
   */
  private var knownUsers = Set[User]()

  /**
   * Generates keys and starts Bluetooth interface.
   *
   * @param additionalInterfaces Instances of [[TransmissionInterface]] to transfer data over
   *                             platform specific interfaces (eg Bluetooth).
   */
  def start(additionalInterfaces: Set[TransmissionInterface] = Set()): Future[Unit] = {
    additionalInterfaces.foreach(transmissionInterfaces += _)
    FutureHelper {
      crypto.generateLocalKeys()
      logger.info("Service started, address is " + crypto.localAddress)
      logger.info("Local user is " + settings.get(SettingsInterface.KeyUserName, "none") +
        " with status '" + settings.get(SettingsInterface.KeyUserStatus, "") + "'")
      transmissionInterfaces +=
        new InternetInterface(this, crypto, settings, maxInternetConnections, port)
      transmissionInterfaces.foreach(_.create())
    }
  }

  def stop(): Unit = {
    transmissionInterfaces.foreach(_.destroy())
    database.close()
  }

  /**
   * Sends a new message to the given target address.
   */
  def sendTo(target: Address, body: MessageBody): Unit = {
    assert(body.contentType != -1)
    FutureHelper {
      val messageId = settings.get("message_id", 0L)
      val header = new ContentHeader(crypto.localAddress, target, seqNumGenerator.next(),
        body.contentType, Some(messageId), Some(new Date()))
      settings.put("message_id", messageId + 1)

      val msg = new Message(header, body)
      val encrypted = crypto.encryptAndSign(msg)
      router.forwardMessage(encrypted)
      onNewMessage(msg)
    }
  }

  private def requestRoute(target: Address): Unit = {
    assert(localRoutesInfo.getRoute(target).isEmpty)
    val seqNum = seqNumGenerator.next()
    // TODO: what params for targSeqNum, originMetric???
    // TODO: when forwarding, keep header.origin, also keep orig* params?
    val body = new RouteRequest(target, seqNum, 0, 0)
    val header = new MessageHeader(body.protocolType, crypto.localAddress, Address.Broadcast, seqNum)

    val msg = new Message(header, body)
    // TODO: we can probably check this before constructing the message
    if (routeMessageInfo.isMessageRedundant(msg))
      return

    val signed = crypto.sign(msg)
    connections().foreach(sendVia(_, signed))
  }

  def replyRoute(target: Address, replyTo: Address): Unit = {
    // TODO: params?
    val body = new RouteReply(target, 0, 0)
    val header = new MessageHeader(body.protocolType, crypto.localAddress, replyTo, seqNumGenerator.next())

    val msg = new Message(header, body)
    val signed = crypto.sign(msg)
    connections().foreach(sendVia(_, signed))
  }

  /**
   * Force connect to a sepcific internet.
   *
   * @param address An address in the format IP;port or hostname:port.
   */
  def connect(address: String): Unit = {
    transmissionInterfaces
      .find(_.isInstanceOf[InternetInterface])
      .map(_.asInstanceOf[InternetInterface])
      .foreach(_.openConnection(address))
  }

  private def sendVia(nextHop: Address, msg: Message) = {
    transmissionInterfaces.foreach(_.send(nextHop, msg))
  }

  /**
   * Decrypts and verifies incoming messages, forwards valid ones to [[onNewMessage()]].
   */
  def onMessageReceived(msg: Message): Unit = {
    if (router.isMessageSeen(msg)) {
      logger.trace("Ignoring message from " + msg.header.origin + " that we already received")
      return
    }

    msg.body match {
      case rreq: RouteRequest =>
        logger.debug(s"rreq: $msg")
        if (localRoutesInfo.getRoute(rreq.requested).isDefined)
          replyRoute(rreq.requested, msg.header.origin)
        else
          requestRoute(rreq.requested)
        onNewMessage(msg)
        return
      case rrep: RouteReply =>
        logger.debug(s"rrep: $msg")
        // TODO: forward this to node that requested it (checking RouteMessageInfo)
        //       -> probably the reason there are way too many rreps
        // TODO: how to handle metric?
        localRoutesInfo.addRoute(rrep.targAddress, rrep.targSeqNum, msg.header.origin, 0)
        connections().foreach(replyRoute(rrep.targAddress, _))
        // resend messages that required this
        if (missingRouteMessages.nonEmpty) {
          logger.debug(s"missingRouteMessages=$missingRouteMessages")
          val m = missingRouteMessages.filter(_.header.target == rrep.targAddress)
          logger.info(s"Now sending messages $m")
          m.foreach(router.forwardMessage)
          missingRouteMessages --= m
        }
        onNewMessage(msg)
        return
      case rerr: RouteError =>
        localRoutesInfo.invalidateRoute(rerr.address)
        connections().foreach(sendTo(_, rerr))
      case _ =>
    }

    if (msg.header.target != crypto.localAddress) {
      router.forwardMessage(msg)
      return
    }

    val plainMsg =
      try {
        if (!crypto.verify(msg)) {
          logger.warn(s"Received message with invalid signature from ${msg.header.origin}")
          return
        }

        if (msg.header.isContentMessage)
          crypto.decrypt(msg)
        else
          msg
      } catch {
        case e: InvalidKeyException =>
          logger.warn(s"Failed to verify or decrypt message $msg", e)
          return
      }

    logger.debug(s"received message $plainMsg")
    onNewMessage(plainMsg)
  }

  private def noRouteFound(message: Message): Unit = {
    logger.debug(s"no route found on node ${crypto.localAddress.toString.split("-").head}")
    missingRouteMessages += message
    requestRoute(message.header.target)
  }

  /**
   * Handles all (locally and remotely sent) new messages.
   */
  private def onNewMessage(msg: Message): Unit = msg.body match {
    case ui: UserInfo =>
      val contact = new User(msg.header.origin, ui.name, ui.status)
      knownUsers += contact
      if (database.getContact(msg.header.origin).nonEmpty)
        database.updateContact(contact)

      callbacks.onConnectionsChanged()
    case _ =>
      val origin = msg.header.origin
      if (origin != crypto.localAddress && database.getContact(origin).isEmpty)
        database.addContact(getUser(origin))

      database.onMessageReceived(msg)
      callbacks.onMessageReceived(msg)
  }

  /**
   * Opens connection to a direct neighbor.
   *
   * This adds the other node's public key if we don't have it. If we do, it validates the signature
   * with the stored key.
   *
   * @param msg The message containing [[ConnectionInfo]] to open the connection.
   * @return True if the connection is valid
   */
  def onConnectionOpened(msg: Message): Boolean = {
    val maxConnections = settings.get(SettingsInterface.KeyMaxConnections,
      SettingsInterface.DefaultMaxConnections.toString).toInt
    if (connections().size == maxConnections) {
      logger.info("Maximum number of connections reached")
      return false
    }

    val info = msg.body.asInstanceOf[ConnectionInfo]
    val sender = crypto.calculateAddress(info.key)
    if (sender == Address.Broadcast || sender == Address.Null) {
      logger.info("Ignoring ConnectionInfo message with invalid sender " + sender)
      return false
    }

    if (crypto.havePublicKey(sender) && !crypto.verify(msg, Option(crypto.getPublicKey(sender)))) {
      logger.info("Ignoring ConnectionInfo message with invalid signature")
      return false
    }

    synchronized {
      if (!crypto.havePublicKey(sender)) {
        crypto.addPublicKey(sender, info.key)
        logger.info("Added public key for new device " + sender.toString)
      }
    }

    // Log with username if we know it.
    if (allKnownUsers().map(_.address).contains(sender))
      logger.info("Node " + getUser(sender).name + " (" + sender + ") connected")
    else
      logger.info("Node " + sender + " connected")

    sendTo(sender, new UserInfo(settings.get(SettingsInterface.KeyUserName, ""),
                                settings.get(SettingsInterface.KeyUserStatus, "")))
    callbacks.onConnectionsChanged()
    true
  }

  def onConnectionClosed(address: Address): Unit = {
    localRoutesInfo.invalidateRoute(address)
    callbacks.onConnectionsChanged()
  }

  def connections(): Set[Address] = transmissionInterfaces.flatMap(_.getConnections)

  private def allKnownUsers() = database.getContacts ++ knownUsers

  /**
   * Returns [[User]] object containing the user's name (if we know it).
   */
  def getUser(address: Address) =
    allKnownUsers()
      .find(_.address == address)
      .getOrElse(new User(address, address.toString(), ""))

  def internetConnectionChanged(): Unit = {
    transmissionInterfaces
      .find(_.isInstanceOf[InternetInterface])
      .foreach(_.asInstanceOf[InternetInterface].connectionChanged())
  }
}
