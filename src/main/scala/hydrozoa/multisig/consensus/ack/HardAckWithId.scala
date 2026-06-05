package hydrozoa.multisig.consensus.ack

/** A coil peer's hard-ack relayed by a hub head peer, stamped with a hub-local [[HubHardAckNumber]]
  * so it travels the head-peer mesh and the hub→coil links on a contiguous per-link lane (the
  * `HubHardAckLane`, §8 of `design/coil-network.md`).
  *
  * The sequence number is transport ordering only — produced by the hub's `CoilAckSequencer`.
  * [[ack]] is the coil peer's full signed hard-ack, verified end-to-end against its `coilPeerVKey`
  * by the receiving `SlowConsensusActor` (the hub is never trusted to vouch for it).
  */
final case class HardAckWithId(
    seqNum: HubHardAckNumber,
    ack: HardAck
)
