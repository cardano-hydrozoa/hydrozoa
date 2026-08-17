package hydrozoa.config.node.operation.evacuation

import hydrozoa.lib.cardano.scalus.QuantizedTime.given
import io.circe.*
import io.circe.generic.semiauto.*
import scala.concurrent.duration.FiniteDuration

final case class NodeOperationEvacuationConfig(
    override val evacuationBotPollingPeriod: FiniteDuration
) extends NodeOperationEvacuationConfig.Section {
    override transparent inline def nodeOperationEvacuationConfig: NodeOperationEvacuationConfig =
        this
}

object NodeOperationEvacuationConfig {
    trait Section {
        def nodeOperationEvacuationConfig: NodeOperationEvacuationConfig

        def evacuationBotPollingPeriod: FiniteDuration =
            nodeOperationEvacuationConfig.evacuationBotPollingPeriod
    }

    given nodeOperationEvacuationConfigEncoder: Encoder[NodeOperationEvacuationConfig] =
        deriveEncoder[NodeOperationEvacuationConfig]

    given nodeOperationEvacuationConfigDecoder: Decoder[NodeOperationEvacuationConfig] =
        Decoder.instance(c =>
            c.downField("evacuationBotPollingPeriod")
                .as[FiniteDuration]
                .map(NodeOperationEvacuationConfig(_))
        )
}
