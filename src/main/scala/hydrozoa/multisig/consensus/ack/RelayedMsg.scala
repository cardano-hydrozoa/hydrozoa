package hydrozoa.multisig.consensus.ack

import hydrozoa.multisig.consensus.UserRequestWithId

/** A fast-side artifact a hub relays to one of its coil peers on the contiguous `relayedMsg` lane
  * (§8 of `design/coil-network.md`), stamped with a hub-local [[RelayedMsgNumber]] for transport
  * ordering.
  *
  * The coil link multiplexes the whole population's stream over a single physical link; the
  * receiving coil peer **de-multiplexes by type and embedded author** — [[Soft]] to its
  * `FastConsensusActor`, [[Hard]] to its `SlowConsensusActor`, [[Req]] to its `BlockWeaver` — which
  * reconstructs the same per-author lane structure a head peer keeps (the basis for shared
  * crash-recovery). The hub is never trusted to vouch for the artifact: it is fully signed and
  * verified end-to-end downstream.
  */
sealed trait RelayedMsg {
    def seqNum: RelayedMsgNumber
}

object RelayedMsg {

    /** A relayed soft-ack (authored by some head peer). */
    final case class Soft(seqNum: RelayedMsgNumber, ack: SoftAck) extends RelayedMsg

    /** A relayed hard-ack (authored by a head or coil peer). */
    final case class Hard(seqNum: RelayedMsgNumber, ack: HardAck) extends RelayedMsg

    /** A relayed user request — the coil needs the request content (not just the brief's ids) to
      * reproduce block bodies and derive `BlockResult`s.
      */
    final case class Req(seqNum: RelayedMsgNumber, request: UserRequestWithId) extends RelayedMsg
}
