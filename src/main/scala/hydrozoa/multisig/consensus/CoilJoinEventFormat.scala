package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.CoilJoinEvent.*
import hydrozoa.multisig.consensus.peer.CoilPeerNumber

/** Renderer from [[CoilJoinEvent]] to [[LogEvent]], routed under `CoilJoin`. */
object CoilJoinEventFormat:

    def humanFormat(own: CoilPeerNumber)(e: CoilJoinEvent): LogEvent = {
        val label = s"c${own: Int}"
        val ev = LogEvent.From(Map("peer" -> label), "CoilJoin")
        import ev.*
        e match {
            case Adopting(startStack) =>
                info(s"adopting a start point at stack=$startStack")
            case Adopted(startStack) =>
                info(s"start point at stack=$startStack verified and stored")
            case NothingToAdopt(reason) =>
                info(s"hub has no start point to offer ($reason); booting from own store")
            case HubSilent(waited) =>
                warn(
                  s"no answer from the hub in $waited; booting on existing history and catching " +
                      "up over the population lanes"
                )
            case StillWaiting =>
                warn(
                  "empty store and no answer from the hub yet — waiting rather than " +
                      "bootstrapping stack 0, which this coil could never recover from"
                )
        }
    }
