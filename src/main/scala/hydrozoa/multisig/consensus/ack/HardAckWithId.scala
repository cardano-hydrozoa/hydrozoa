package hydrozoa.multisig.consensus.ack

import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** A coil peer's hard-ack relayed by a hub head peer, stamped with a hub-local [[HubHardAckNumber]]
  * so it travels the head-peer mesh and the hub→coil links on a contiguous per-hub lane (the
  * `HubHardAckLane`, §5.3 of `design/coil-network.md`) [doc-ref].
  *
  * [[hubPeer]] is the head peer that re-sequenced this ack — the entry is intrinsically scoped to
  * one hub, so the per-hub `HubHardAck` lane it belongs to is self-described (a coil peer learns
  * each hub's lane via its single hub forwarding all of them). [[seqNum]] is that hub's transport
  * ordering only. [[ack]] is the coil peer's full signed hard-ack, verified end-to-end against its
  * `coilPeerVKey` by the receiving `SlowConsensusActor` (the hub is never trusted to vouch for it).
  */
final case class HardAckWithId(
    hubPeer: HeadPeerNumber,
    seqNum: HubHardAckNumber,
    ack: HardAck
)
