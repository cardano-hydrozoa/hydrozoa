package hydrozoa.multisig.consensus

import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.RequestNumber

/** A node-local notification the [[FastConsensusActor]] fans out to its [[RequestSequencer]] and
  * its mesh [[liaison.PeerLiaisonHeadToHead]]s when a block soft-confirms: the per-author
  * high-water request number carried by that block.
  *
  * Recipients merge it into their own confirmed-high-water view by max (a block carries only the
  * authors that appear in it). It anchors request backpressure — the sequencer refuses to author,
  * and each mesh puller refuses to pull, more than `backpressureCoefficient * maxRequestsPerBlock`
  * beyond a peer's confirmed high-water — so the mempool cannot exceed
  * `backpressureCoefficient * maxRequestsPerBlock * nHeadPeers`. The pull ceiling scales in
  * lockstep with the admission window so a follower can always pull as far ahead as any leader may
  * sequence (a leader packs its prioritized own requests anywhere in that window). See
  * docs/spec/fast-consensus.md.
  */
final case class SoftConfirmedHighWater(highWater: Map[HeadPeerNumber, RequestNumber])
