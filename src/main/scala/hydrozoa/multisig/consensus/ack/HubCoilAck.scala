package hydrozoa.multisig.consensus.ack

/** A coil hard-ack relayed by a hub head, stamped with a hub-local [[HubCoilAckNumber]] so it
  * travels the head mesh and the hub→coil links on a contiguous per-link lane (the
  * `HubCoilAckLane`, §8 of `design/coil-network.md`).
  *
  * The sequence number is transport ordering only — produced by the hub's `CoilAckSequencer`.
  * [[ack]] is the coil's full signed hard-ack, verified end-to-end against its `coilPeerVKey` by
  * the receiving `SlowConsensusActor` (the hub is never trusted to vouch for it).
  */
final case class HubCoilAck(
    seqNum: HubCoilAckNumber,
    ack: HardAck
)
