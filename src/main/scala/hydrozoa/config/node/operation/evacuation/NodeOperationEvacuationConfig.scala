package hydrozoa.config.node.operation.evacuation

import hydrozoa.lib.cardano.scalus.QuantizedTime.given
import hydrozoa.multisig.consensus.peer.PeerWallet
import io.circe.*
import io.circe.generic.semiauto.*
import scala.concurrent.duration.FiniteDuration

final case class NodeOperationEvacuationConfig(
    override val evacuationBotPollingPeriod: FiniteDuration,
    override val ruleBasedWallet: PeerWallet
) extends NodeOperationEvacuationConfig.Section {
    override transparent inline def nodeOperationEvacuationConfig: NodeOperationEvacuationConfig =
        this
}

object NodeOperationEvacuationConfig {
    trait Section {
        def nodeOperationEvacuationConfig: NodeOperationEvacuationConfig

        def evacuationBotPollingPeriod: FiniteDuration =
            nodeOperationEvacuationConfig.evacuationBotPollingPeriod

        def ruleBasedWallet: PeerWallet = nodeOperationEvacuationConfig.ruleBasedWallet
    }

    given nodeOperationEvacuationConfigEncoder: Encoder[NodeOperationEvacuationConfig] =
        deriveEncoder[NodeOperationEvacuationConfig]

    given nodeOperationEvacuationConfigDecoder: Decoder[NodeOperationEvacuationConfig] =
        Decoder.instance(c =>
            for {
                ebpp <- c.downField("evacuationBotPollingPeriod").as[FiniteDuration]
                rbw <- c.downField("ruleBasedWallet").as[PeerWallet]
            } yield NodeOperationEvacuationConfig(ebpp, rbw)
        )
}
