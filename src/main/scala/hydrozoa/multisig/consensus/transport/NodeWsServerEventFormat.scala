package hydrozoa.multisig.consensus.transport

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.transport.NodeWsServerEvent.*

/** Renderers from [[NodeWsServerEvent]] to [[LogEvent]]. */
object NodeWsServerEventFormat:

    /** Routes under `NodeWsServer.<own>` — tune the server's events for one peer at once. */
    def humanFormat(own: HeadPeerNumber)(e: NodeWsServerEvent): LogEvent = {
        val ownPn: Int = own
        val ev = LogEvent.From(Map("peer" -> ownPn.toString), s"NodeWsServer.$ownPn")
        import ev.*
        e match {
            case Bound(host, port) =>
                info(s"WS server bound at ws://$host:$port")
        }
    }
