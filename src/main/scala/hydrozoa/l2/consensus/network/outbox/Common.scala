package hydrozoa.l2.consensus.network.outbox

import hydrozoa.l2.consensus.network.{Ack, Req}

// TODO: find a better place?
opaque type PeerId = String

// Common stuff for Outbox*

/** Requests or acknowledge that originate from "this" peer and will be broadcast to all other peers
  */
case class OutMsg(outMsgId: OutMsgId, content: Req | Ack)

// Surrogate primary key for outgoing messages, starts with 1.
opaque type OutMsgId = Long

object OutMsgId:
    def apply(n: Long): OutMsgId = {
        assert(n > 0, "OutMsgIds must be positive")
        n
    }

    extension (self: OutMsgId) {
        def nextMsgId: OutMsgId = self.toLong + 1
        def toLong: Long = self
    }

// Surrogate primary key for incoming messages, starts with 1.
opaque type InMsgId = (PeerId, OutMsgId)

object InMsgId:
    def apply(peer: PeerId, n: OutMsgId): InMsgId = {
        assert(n > 0, "InMsgId must be positive")
        (peer, n)
    }

/** Matching index for a remote peer i.e., the position in the outbox that is confirmed by a
  * recipient. It is a non-negative Long under the hood. A MatchIndex of 0 indicates that the remote
  * peer has not told us about their match index yet.
  */
opaque type MatchIndex = Long

object MatchIndex:
    def apply(n: Long): MatchIndex = {
        assert(n >= 0, "Match Indicies must be non-negative")
        n
    }

    extension (self: MatchIndex) {
        def nextMsgId: OutMsgId = self.toLong + 1
        def toLong: Long = self
    }
