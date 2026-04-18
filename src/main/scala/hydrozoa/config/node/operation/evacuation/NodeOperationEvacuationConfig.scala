package hydrozoa.config.node.operation.evacuation

import hydrozoa.multisig.consensus.peer.HeadPeerWallet
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

        def evacuationBotPollingPeriod: FiniteDuration =
            nodeOperationEvacuationConfig.evacuationBotPollingPeriod

        def evacuationWallet: HeadPeerWallet = nodeOperationEvacuationConfig.evacuationWallet
    }
}
