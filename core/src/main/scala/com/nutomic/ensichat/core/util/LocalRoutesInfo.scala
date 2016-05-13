package com.nutomic.ensichat.core.util

import java.util.Date

import com.nutomic.ensichat.core.Address

import scala.concurrent.duration._
import com.nutomic.ensichat.core.util.LocalRoutesInfo._

object LocalRoutesInfo {

  private val ActiveInterval = 5.seconds

  /**
    * [[RouteStates.Idle]]:
    *     A route that is known, but has not been used in the last [[ActiveInterval.
    * [[RouteStates.Active]]:
    *     A route that is known, and has been used in the last [[ActiveInterval]].
    * [[RouteStates.Invalid]]:
    *     A route that has been expired or lost, may not be used for forwarding.
    * RouteStates.Unconfirmed is not required as connections are always bidirectional.
    */
  object RouteStates extends Enumeration {
    type RouteStates = Value
    val Idle, Active, Invalid = Value
  }

}

/**
  * This class contains information about routes available to this node.
  *
  * See AODVv2-13 4.5 (Local Route Set),              -> implemented
  *               6.9 (Local Route Set Maintenance)   -> implemented (hopefully correct)
  */
class LocalRoutesInfo(activeConnections: () => Set[Address]) {

  import RouteStates._

  private val MaxSeqnumLifetime = 300.seconds
  // TODO: this can probably be much higher because of infrequent topology changes between internet nodes
  private val MaxIdleTime = 300.seconds


  /**
   *  Holds information about a local route.
   *
   * @param destination The destination address that can be reached with this route.
   * @param seqNum Sequence number of the last route message that updated this entry.
   * @param nextHop The next hop on the path towards destination.
   * @param lastUsed The time this route was last used to forward a message.
   * @param lastSeqNumUpdate The time seqNum was last updated.
   * @param metric The number of hops towards destination using this route.
   * @param state The last known state of the route.
   */
  case class RouteEntry(destination: Address, seqNum: Int, nextHop: Address, lastUsed: Date,
                        lastSeqNumUpdate: Date, metric: Int, state: RouteStates)

  private var routes = Set[RouteEntry]()

  def addRoute(destination: Address, seqNum: Int, nextHop: Address, metric: Int): Unit = {
    val entry = RouteEntry(destination, seqNum, nextHop, new Date(0), new Date(), metric, Idle)
    routes += entry
  }

  def getRoute(destination: Address): Option[RouteEntry] = {
    if (activeConnections().contains(destination))
      return Option(new RouteEntry(destination, 0, destination, new Date(), new Date(), 1, Idle))

    handleTimeouts()
    val r = routes.find(_.destination == destination)
    if (r.isDefined)
      routes = routes -- r + r.get.copy(state = Active, lastUsed = new Date())
    r
  }

  /**
    *
    * @param neighbor The address of a neighbor which has disconnected.
    * @return The set of active destinations that can't be reached anymore.
    */
  def connectionClosed(neighbor: Address): Set[Address] = {
    handleTimeouts()
    routes = routes.map { r =>
      if (r.nextHop == neighbor)
        r.copy(state = Invalid)
      else
        r
    }

    routes
      .filter(r => r.state == Active && r.nextHop == neighbor)
      .map(_.destination)
  }

  private def handleTimeouts(): Unit = {
    routes = routes
      // Delete routes after max lifetime.
      .map { r =>
        if (r.lastSeqNumUpdate.getTime + MaxSeqnumLifetime.toMillis > new Date().getTime)
          r.copy(seqNum = 0)
        else
          r
      }
      // Set routes to invalid after max idle time.
      .map { r =>
        if (r.lastSeqNumUpdate.getTime + MaxIdleTime.toMillis < new Date().getTime)
          r.copy(state = Invalid)
        else
          r
      }
  }

}