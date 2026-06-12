package hydrozoa.multisig.consensus.transport

import com.comcast.ip4s.{Host, Port}

/** Typed events emitted by [[NodeWsServer]]. Pure data; formatters in [[NodeWsServerEventFormat]]
  * decide how each variant is rendered to a particular sink.
  */
sealed trait NodeWsServerEvent

object NodeWsServerEvent:
    /** Emitted once when the Ember server successfully binds its listening socket. */
    final case class Bound(host: Host, port: Port) extends NodeWsServerEvent
