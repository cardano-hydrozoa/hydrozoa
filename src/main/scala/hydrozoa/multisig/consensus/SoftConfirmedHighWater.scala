package hydrozoa.multisig.consensus

import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.RequestNumber

/** A node-local notification the [[FastConsensusActor]] fans out to its [[RequestSequencer]] and
  * its mesh [[liaison.PeerLiaisonHeadToHead]]s when a block soft-confirms: the per-author
  * high-water request number carried by that block.
  *
  * Recipients merge it into their own confirmed-high-water view by max (a block carries only the
  * authors that appear in it). It anchors request backpressure — the sequencer refuses to author
  * more than `maxRequestsPerBlock` beyond its own confirmed high-water, and each mesh puller caps
  * its request pull at `confirmedHighWater(remote) + maxRequestsPerBlock` — so the mempool cannot
  * exceed `maxRequestsPerBlock * nHeadPeers`. See docs/spec/fast-consensus.md.
  */
final case class SoftConfirmedHighWater(highWater: Map[HeadPeerNumber, RequestNumber])
