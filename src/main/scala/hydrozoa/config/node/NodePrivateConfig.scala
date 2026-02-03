package hydrozoa.config.node

import hydrozoa.config.node.operation.liquidation.NodeOperationLiquidationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.multisig.consensus.peer.HeadPeerWallet
import scala.concurrent.duration.FiniteDuration

final case class NodePrivateConfig(
    override val ownHeadPeer: OwnHeadPeer,
    override val nodeOperationLiquidationConfig: NodeOperationLiquidationConfig,
    override val nodeOperationMultisigConfig: NodeOperationMultisigConfig,
) extends NodePrivateConfig.Section {
    override transparent inline def nodePrivateConfig: NodePrivateConfig = this
}

object NodePrivateConfig {
    trait Section
        extends NodeOperationMultisigConfig.Section,
          NodeOperationLiquidationConfig.Section,
          OwnHeadPeer.Section {
        def nodePrivateConfig: NodePrivateConfig

        override transparent inline def ownHeadWallet: HeadPeerWallet = ownHeadPeer.ownHeadWallet

        override transparent inline def liquidationBotPollingPeriod: FiniteDuration =
            nodeOperationLiquidationConfig.liquidationBotPollingPeriod

        override transparent inline def cardanoLiaisonPollingPeriod: FiniteDuration =
            nodeOperationMultisigConfig.cardanoLiaisonPollingPeriod
    }
}
