package hydrozoa.config.node.operation.multisig

import hydrozoa.lib.cardano.scalus.QuantizedTime.given
import hydrozoa.lib.number.PositiveInt
import io.circe.*
import io.circe.generic.semiauto.*
import scala.concurrent.duration.{DurationInt, FiniteDuration}

final case class NodeOperationMultisigConfig(
    override val cardanoLiaisonPollingPeriod: FiniteDuration,
    override val peerLiaisonMaxRequestsPerBatch: PositiveInt,
    override val peerLiaisonOutboxCap: PositiveInt,
    override val peerLiaisonResendInterval: FiniteDuration
) extends NodeOperationMultisigConfig.Section {
    override transparent inline def nodeOperationMultisigConfig: NodeOperationMultisigConfig = this
}

object NodeOperationMultisigConfig {
    trait Section {
        def nodeOperationMultisigConfig: NodeOperationMultisigConfig

        def cardanoLiaisonPollingPeriod: FiniteDuration =
            nodeOperationMultisigConfig.cardanoLiaisonPollingPeriod
        def peerLiaisonMaxRequestsPerBatch: PositiveInt =
            nodeOperationMultisigConfig.peerLiaisonMaxRequestsPerBatch

        /** How many items each [[hydrozoa.multisig.consensus.liaison.LaneOutbound]] keeps in
          * memory, floored at that lane's `maxPerReply`. Above the cap the oldest is evicted; a
          * pull below the remaining floor is served from the journal instead, which is sound
          * because nothing reaches a lane before it is durable (CR4) and is already how every lane
          * serves after a restart.
          *
          * Node-local on purpose: it changes only how much this peer caches, never what it sends,
          * so peers may run different values without diverging.
          */
        def peerLiaisonOutboxCap: PositiveInt =
            nodeOperationMultisigConfig.peerLiaisonOutboxCap

        /** How often each [[hydrozoa.multisig.consensus.PeerLiaisonHeadToHead]] re-sends its
          * currently outstanding `GetMsgBatch` to the remote peer, to recover from a stalled
          * request-response chain (e.g. caused by a dropped WS frame). The re-send is idempotent on
          * the wire.
          */
        def peerLiaisonResendInterval: FiniteDuration =
            nodeOperationMultisigConfig.peerLiaisonResendInterval
    }

    /** Two `peerLiaisonMaxRequestsPerBatch` batches of requests, which is also generous headroom on
      * the lanes that reply one item at a time. At the market-maker payload mix a request lane then
      * holds tens of MB rather than everything the process has ever relayed.
      */
    val defaultPeerLiaisonOutboxCap: PositiveInt = PositiveInt.unsafeApply(1024)

    lazy val default: NodeOperationMultisigConfig = NodeOperationMultisigConfig(
      cardanoLiaisonPollingPeriod = 10.seconds,
      peerLiaisonMaxRequestsPerBatch = PositiveInt.unsafeApply(500),
      peerLiaisonOutboxCap = defaultPeerLiaisonOutboxCap,
      peerLiaisonResendInterval = 5.seconds
    )

    given Encoder[NodeOperationMultisigConfig] = deriveEncoder[NodeOperationMultisigConfig]

    /** Hand-written rather than derived so `peerLiaisonOutboxCap` may be **absent**: every config
      * file written before this field existed must still decode, and a node whose config fails to
      * decode does not start at all.
      */
    given Decoder[NodeOperationMultisigConfig] = Decoder.instance(c =>
        for {
            pollingPeriod <- c.downField("cardanoLiaisonPollingPeriod").as[FiniteDuration]
            maxRequestsPerBatch <- c.downField("peerLiaisonMaxRequestsPerBatch").as[PositiveInt]
            outboxCap <- c.downField("peerLiaisonOutboxCap").as[Option[PositiveInt]]
            resendInterval <- c.downField("peerLiaisonResendInterval").as[FiniteDuration]
        } yield NodeOperationMultisigConfig(
          cardanoLiaisonPollingPeriod = pollingPeriod,
          peerLiaisonMaxRequestsPerBatch = maxRequestsPerBatch,
          peerLiaisonOutboxCap = outboxCap.getOrElse(defaultPeerLiaisonOutboxCap),
          peerLiaisonResendInterval = resendInterval
        )
    )
}
