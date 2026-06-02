package hydrozoa.config.node.operation.multisig

import hydrozoa.lib.cardano.scalus.QuantizedTime.given
import hydrozoa.lib.number.PositiveInt
import io.circe.*
import io.circe.generic.semiauto.*
import scala.concurrent.duration.{DurationInt, FiniteDuration}

final case class NodeOperationMultisigConfig(
    override val cardanoLiaisonPollingPeriod: FiniteDuration,
    override val peerLiaisonMaxRequestsPerBatch: PositiveInt,
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

        /** How often each [[hydrozoa.multisig.consensus.PeerLiaison]] re-sends its currently
          * outstanding `GetMsgBatch` to the remote peer, to recover from a stalled request-response
          * chain (e.g. caused by a dropped WS frame). The re-send is idempotent on the wire.
          */
        def peerLiaisonResendInterval: FiniteDuration =
            nodeOperationMultisigConfig.peerLiaisonResendInterval

        override def rateLimits: RateLimits = nodeOperationMultisigConfig.rateLimits
    }

    lazy val default = NodeOperationMultisigConfig(
      cardanoLiaisonPollingPeriod = 10.seconds,
      peerLiaisonMaxRequestsPerBatch = PositiveInt.unsafeApply(42),
      peerLiaisonResendInterval = 5.seconds,
      rateLimits = RateLimits.default
    )

    given Encoder[NodeOperationMultisigConfig] = deriveEncoder[NodeOperationMultisigConfig]

    given Decoder[NodeOperationMultisigConfig] = deriveDecoder[NodeOperationMultisigConfig]
}
