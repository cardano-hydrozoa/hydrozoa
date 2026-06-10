package hydrozoa.multisig.consensus.liaison

import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckNumber, HardAckWithId, HubHardAckNumber, SoftAck, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}

/** The batch messages for the three liaison links — one set per link (§5.5 of
  * `design/coil-network.md`) [doc-ref]. Each `GetMsgBatch` is a product of **next-expected
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
}
