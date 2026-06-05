package hydrozoa.multisig.consensus.ack

import hydrozoa.multisig.consensus.UserRequestWithId

/** A fast-side artifact a hub relays to one of its coils on the contiguous `relayedAck` lane (§8 of
  * `design/coil-network.md`), stamped with a hub-local [[RelayedAckNumber]] for transport ordering.
  *
  * The coil link multiplexes the whole population's stream over a single physical link; the
  * receiving coil **de-multiplexes by type and embedded author** — [[Soft]] to its
  * `FastConsensusActor`, [[Hard]] to its `SlowConsensusActor`, [[Req]] to its `BlockWeaver` — which
  * reconstructs the same per-author lane structure a head keeps (the basis for shared
  * crash-recovery). The hub is never trusted to vouch for the artifact: it is fully signed and
  * verified end-to-end downstream.
  */
sealed trait RelayedAck {
    def seqNum: RelayedAckNumber
}

object RelayedAck {

    /** A relayed soft-ack (authored by some head peer). */
    final case class Soft(seqNum: RelayedAckNumber, ack: SoftAck) extends RelayedAck

    /** A relayed hard-ack (authored by a head or coil peer). */
    final case class Hard(seqNum: RelayedAckNumber, ack: HardAck) extends RelayedAck

    /** A relayed user request — the coil needs the request content (not just the brief's ids) to
      * reproduce block bodies and derive `BlockResult`s.
      */
    final case class Req(seqNum: RelayedAckNumber, request: UserRequestWithId) extends RelayedAck
}
