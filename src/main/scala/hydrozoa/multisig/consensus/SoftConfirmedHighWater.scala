package hydrozoa.multisig.consensus

import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber

/** A node-local notification the [[FastConsensusActor]] fans out when a block soft-confirms: the
  * block's number, and the per-author high-water request number it carried.
  *
  * Recipients are its [[RequestSequencer]], its mesh [[liaison.PeerLiaisonHeadToHead]]s, and — on a
  * coil peer, where the first two are empty — its [[liaison.PeerLiaisonCoilToHub]].
  *
  * `highWater` is merged into the recipient's own confirmed-high-water view by max (a block carries
  * only the authors that appear in it). It anchors request backpressure — the sequencer refuses to
  * author, and each puller refuses to pull, more than `backpressureCoefficient *
  * maxRequestsPerBlock` beyond a peer's confirmed high-water — so the mempool cannot exceed
  * `backpressureCoefficient * maxRequestsPerBlock * nHeadPeers`. The pull ceiling scales in
  * lockstep with the admission window so a follower can always pull as far ahead as any leader may
  * sequence (a leader packs its prioritized own requests anywhere in that window). See
  * docs/spec/fast-consensus.md.
  *
  * `blockNum` anchors the coil peer's block and soft-ack pull ceilings, which the mesh has no need
  * of: a head peer cannot run ahead of its own soft-confirmation, but a hub serving the whole
  * population to a coil peer can. See docs/spec/liaison-backpressure.md.
  */
final case class SoftConfirmedHighWater(
    blockNum: BlockNumber,
    highWater: Map[HeadPeerNumber, RequestNumber]
)
