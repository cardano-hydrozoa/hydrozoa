package hydrozoa.config.node.operation.evacuation

import hydrozoa.config.head.multisig.timing.given
import hydrozoa.multisig.consensus.peer.HeadPeerWallet
import io.circe.*
import io.circe.generic.semiauto.*
import scala.concurrent.duration.FiniteDuration

final case class NodeOperationEvacuationConfig(
    override val evacuationBotPollingPeriod: FiniteDuration,
    override val evacuationWallet: HeadPeerWallet
) extends NodeOperationEvacuationConfig.Section {
    override transparent inline def nodeOperationEvacuationConfig: NodeOperationEvacuationConfig =
        this
}

object NodeOperationEvacuationConfig {
    trait Section {
        def nodeOperationEvacuationConfig: NodeOperationEvacuationConfig

        def evacuationBotPollingPeriod: FiniteDuration

        def evacuationWallet: HeadPeerWallet
    }

    given nodeOperationEvacuationConfigEncoder: Encoder[NodeOperationEvacuationConfig] =
        deriveEncoder[NodeOperationEvacuationConfig]

    given nodeOperationEvacuationConfigDecoder: Decoder[NodeOperationEvacuationConfig] =
        Decoder.instance(c =>
            for {
                ebpp <- c.downField("evacuationBotPollingPeriod").as[FiniteDuration]
                ew <- c.downField("evacuationWallet").as[HeadPeerWallet]
            } yield NodeOperationEvacuationConfig(ebpp, ew)
        )
}
