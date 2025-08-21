package hydrozoa.l2.consensus.network.outbox

import hydrozoa.l2.consensus.network.{Ack, Req}

// Common stuff for Outbox*

/** Requests or acknowledge that originate from "this" peer and will be broadcast to all other peers */
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

// Matching index i.e., the position in the outbox that is confirmed by a recipient, starts with 0.
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
