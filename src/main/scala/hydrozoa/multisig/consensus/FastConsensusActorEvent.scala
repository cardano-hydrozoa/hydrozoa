package hydrozoa.multisig.consensus

import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber

/** Typed events emitted by [[FastConsensusActor]]. Pure data; formatters in
  * [[FastConsensusActorEventFormat]] decide how each variant is rendered to a particular sink.
  *
  * [[LeaderStarted]] is the one variant production never emits — it exists for stage1's
  * `BlockWeaverMock` stub, which emits it on FCA's behalf so the conformance trace contains
  * leader-started lines.
  */
sealed trait FastConsensusActorEvent

object FastConsensusActorEvent:

    final case class AckReceived(
        blockNum: BlockNumber,
        peerNum: HeadPeerNumber,
        ackType: String,
        isOwn: Boolean
    ) extends FastConsensusActorEvent

    final case class BlockSoftConfirmed(
        blockNum: BlockNumber,
        blockType: String,
        vMajor: Int,
        vMinor: Int
    ) extends FastConsensusActorEvent

    /** Emitted by leader-side stubs / wrappers when a block leadership window begins. */
    final case class LeaderStarted(blockNum: BlockNumber, peer: HeadPeerNumber)
        extends FastConsensusActorEvent
