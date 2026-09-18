package hydrozoa.config.node.operation.multisig

import hydrozoa.lib.cardano.scalus.QuantizedTime.given
import hydrozoa.lib.number.PositiveInt
import io.circe.*
import io.circe.generic.semiauto.*
import scala.concurrent.duration.{DurationInt, FiniteDuration}

final case class NodeOperationMultisigConfig(
    override val cardanoLiaisonPollingPeriod: FiniteDuration,
    override val peerLiaisonMaxRequestsPerBatch: PositiveInt,
    override val peerLiaisonOutboxDepth: PositiveInt,
    override val peerLiaisonResendInterval: FiniteDuration,
    override val rateLimits: RateLimits
) extends NodeOperationMultisigConfig.Section {
    override transparent inline def nodeOperationMultisigConfig: NodeOperationMultisigConfig = this
}

object NodeOperationMultisigConfig {
    trait Section extends RateLimits.Section {
        def nodeOperationMultisigConfig: NodeOperationMultisigConfig

        def cardanoLiaisonPollingPeriod: FiniteDuration =
            nodeOperationMultisigConfig.cardanoLiaisonPollingPeriod
        def peerLiaisonMaxRequestsPerBatch: PositiveInt =
            nodeOperationMultisigConfig.peerLiaisonMaxRequestsPerBatch

        /** How many *replies* each [[hydrozoa.multisig.consensus.liaison.LaneOutbound]] keeps in
          * memory. A lane caches `peerLiaisonOutboxDepth * maxPerReply` items, so one value means
          * the same slack on every lane however many items its replies carry. Above that the oldest
          * is evicted; a pull below the remaining floor is served from the journal instead, which
          * is sound because nothing reaches a lane before it is durable (CR4) and is already how
          * every lane serves after a restart.
          *
          * Node-local on purpose: it changes only how much this peer caches, never what it sends,
          * so peers may run different values without diverging.
          */
        def peerLiaisonOutboxDepth: PositiveInt =
            nodeOperationMultisigConfig.peerLiaisonOutboxDepth

        /** How often each [[hydrozoa.multisig.consensus.PeerLiaisonHeadToHead]] re-sends its
          * currently outstanding `GetMsgBatch` to the remote peer, to recover from a stalled
          * request-response chain (e.g. caused by a dropped WS frame). The re-send is idempotent on
          * the wire.
          */
        def peerLiaisonResendInterval: FiniteDuration =
            nodeOperationMultisigConfig.peerLiaisonResendInterval

        override def rateLimits: RateLimits = nodeOperationMultisigConfig.rateLimits
    }

    /** Two replies' worth on every lane: the reply a current remote is about to pull, plus one of
      * slack for the one it pulled last. A remote that falls further behind is served from the
      * journal, which is the same path every lane takes after a restart.
      */
    val defaultPeerLiaisonOutboxDepth: PositiveInt = PositiveInt.unsafeApply(2)

    lazy val default: NodeOperationMultisigConfig = NodeOperationMultisigConfig(
      cardanoLiaisonPollingPeriod = 10.seconds,
      peerLiaisonMaxRequestsPerBatch = PositiveInt.unsafeApply(500),
      peerLiaisonOutboxDepth = defaultPeerLiaisonOutboxDepth,
      peerLiaisonResendInterval = 5.seconds,
      rateLimits = RateLimits.default
    )

    given Encoder[NodeOperationMultisigConfig] = deriveEncoder[NodeOperationMultisigConfig]

    given Decoder[NodeOperationMultisigConfig] = deriveDecoder[NodeOperationMultisigConfig]
}
