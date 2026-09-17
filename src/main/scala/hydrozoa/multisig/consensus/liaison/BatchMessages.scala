package hydrozoa.multisig.consensus.liaison

import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckNumber, HardAckWithId, HubHardAckNumber, SoftAck, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap
import hydrozoa.multisig.ledger.l1.tx.SettlementTx
import hydrozoa.multisig.ledger.l2.L2StateExport
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber, StandaloneEvacuationCommitment}

/** The batch messages for the three liaison links — one set per link (§5.5 of
  * `docs/spec/coil-network.md`) [doc-ref]. Each `GetMsgBatch` is a product of **next-expected
  * cursors**, one per lane the puller wants; each `NewMsgBatch` is the matching product of
  * **payload slices**, one per lane the server holds. There is no fat one-size-fits-all batch —
  * each link carries exactly its own lanes, and lanes that exist per author are `Map`s keyed by the
  * author's [[HeadPeerNumber]].
  *
  * Three lane-set shapes flow on the network:
  *   - [[Mesh]] — head ↔ head: one head peer's own production (six single lanes), symmetric.
  *   - [[Population]] — hub → coil: the whole population (blocks, stacks, and per-author request /
  *     soft-ack / head-hard-ack lanes, plus per-hub coil-hard-ack lanes). The coil peer pulls
  *     `Population.Get`; the hub answers `Population.New`.
  *   - [[OwnHardAck]] — coil → hub: just this coil peer's own hard-ack (one lane). The hub pulls
  *     `OwnHardAck.Get`; the coil peer answers `OwnHardAck.New`.
  */
object BatchMessages {

    /** Head ↔ head: each side serves its **own** production — block + stack briefs (sparse,
      * own-led), its request / soft-ack / head-hard-ack lanes, and (if it hubs coil peers) its one
      * `HubHardAck` lane.
      */
    object Mesh {
        final case class Get(
            batchNum: BatchNumber,
            block: BlockNumber,
            stack: StackNumber,
            request: RequestNumber,
            // Backpressure: the highest request number the puller is willing to accept from this
            // remote author right now — its confirmed high-water for this author plus one block's
            // cap. The server truncates its request slice at this ceiling so the puller never buffers
            // more than one cap of the author's unconfirmed requests (docs/spec/fast-consensus.md).
            requestCeiling: RequestNumber,
            softAck: SoftAckNumber,
            headHardAck: HardAckNumber,
            hubHardAck: HubHardAckNumber
        )

        final case class New(
            batchNum: BatchNumber,
            block: Option[BlockBrief.Next],
            stack: Option[StackBrief],
            requests: List[UserRequestWithId],
            softAck: Option[SoftAck],
            headHardAck: Option[HardAck],
            hubHardAck: Option[HardAckWithId]
        )
    }

    /** Hub → coil: the full population stream, lanes kept separate. Block + stack are single
      * contiguous spines; requests / soft-acks / head-hard-acks are per head-peer author; coil
      * hard-acks are per hub. Pulled by the coil peer, served by the hub.
      */
    object Population {
        final case class Get(
            batchNum: BatchNumber,
            block: BlockNumber,
            stack: StackNumber,
            requests: Map[HeadPeerNumber, RequestNumber],
            softAcks: Map[HeadPeerNumber, SoftAckNumber],
            headHardAcks: Map[HeadPeerNumber, HardAckNumber],
            coilHardAcks: Map[HeadPeerNumber, HubHardAckNumber]
        )

        final case class New(
            batchNum: BatchNumber,
            block: Option[BlockBrief.Next],
            stack: Option[StackBrief],
            requests: Map[HeadPeerNumber, List[UserRequestWithId]],
            softAcks: Map[HeadPeerNumber, Option[SoftAck]],
            headHardAcks: Map[HeadPeerNumber, Option[HardAck]],
            coilHardAcks: Map[HeadPeerNumber, Option[HardAckWithId]]
        )
    }

    /** Coil → hub: just this coil peer's own hard-ack lane. Pulled by the hub, served by the coil
      * peer.
      */
    object OwnHardAck {
        final case class Get(
            batchNum: BatchNumber,
            hardAck: HardAckNumber
        )

        final case class New(
            batchNum: BatchNumber,
            hardAck: Option[HardAck]
        )
    }

    /** Hub ↔ coil: the start point a coil peer is put at when it connects (GUM-312).
      *
      * Not a lane and not a pull chain — a single exchange that runs once per connection, before
      * the coil's puller opens. The coil says where it stands; the hub decides where it starts.
      */
    object Join {

        /** Transport → hub liaison, **local only**: a coil peer's link came up, claiming these
          * marks. `None` for either means the coil holds nothing there.
          *
          * A hint, not a cursor set: `Population.Get` has seven cursor families and the coil
          * enumerates none of them. The hub uses this to pick a start point and then sends back
          * every index itself.
          */
        final case class Connected(
            block: Option[BlockNumber],
            stack: Option[StackNumber]
        )

        /** Hub → coil: where to start, what to start from, and what proves it.
          *
          * **Two signed artifacts, not a `StackEffects`.** Effects are otherwise never
          * wire-broadcast — every peer derives them from its own `BlockResult` stream, which is
          * what makes them byte-identical without being sent. A joining coil has no such stream
          * below its start point, so these two travel and nothing else does: the narrowest
          * exception that still lets the coil check what it adopts. See
          * [[hydrozoa.multisig.ledger.stack.StackEffects]].
          *
          * @param startStack
          *   the hard-confirmed stack whose partition the start point sits in.
          * @param cursors
          *   every population lane's first index, to adopt exactly as sent. `batchNum` is where the
          *   coil's pull chain opens.
          * @param ownHardAck
          *   the first own-hard-ack index the hub will ask this coil for. The hub has already moved
          *   its own inbound cursor here, so the acks below it are never requested — which is what
          *   makes a coil with no history able to connect at all.
          * @param settlement
          *   **the source of the treasury**, and always present: the settlement of the start
          *   point's own partition when that is a major, otherwise the one from the latest major at
          *   or before it — a minor partition touches no L1 and rotates no treasury. Multisigned,
          *   so the coil validates it rather than trusting the hub for it.
          * @param sec
          *   the start point's own SEC, present exactly when the start point is a **minor**
          *   partition. It carries the `l2StateHash` and evacuation commitment at that minor, which
          *   the [[settlement]] — being older — does not. `None` at a major, where the settlement
          *   carries both itself.
          * @param state
          *   the L2 state at the start point, opaque to everyone but the backend that made it.
          * @param block
          *   the brief of the start point's **last block**, one below where [[cursors]] opens. The
          *   coil never pulls this one and cannot produce the next block without it: a block is
          *   built on its predecessor's header.
          * @param deposits
          *   the deposit map as of that same block. Like [[block]] it is fast-side state the coil
          *   carries rather than re-derives — the decisions behind it are spread over the whole
          *   history below the start point.
          */
        final case class Offer(
            startStack: StackNumber,
            cursors: Population.Get,
            ownHardAck: HardAckNumber,
            settlement: SettlementTx,
            sec: Option[StandaloneEvacuationCommitment.MultiSigned],
            state: L2StateExport,
            block: BlockBrief.Next,
            deposits: DepositsMap
        )

        /** Hub → coil: there is no start point to adopt, carry on.
          *
          * The hub answers every handshake, and this is the ordinary answer — a reconnecting coil
          * is nearly always close enough to walk forward. Sending it rather than staying silent is
          * what lets a coil tell "nothing to adopt" from "no hub", which are the same silence and
          * very different situations: the first is a normal boot, the second is a coil about to
          * bootstrap stack 0 into a head that is long past it.
          *
          * `reason` is for the log; the coil does the same thing whatever it says.
          */
        final case class NoOffer(reason: String)

        /** What a hub sends back when a coil announces itself. */
        type Answer = Offer | NoOffer
    }
}
