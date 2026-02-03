package hydrozoa.config.node

import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.HeadParameters
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.operation.liquidation.NodeOperationLiquidationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.multisig.ledger.block.Block

final case class NodeConfig(
    override val headConfig: HeadConfig,
    override val nodePrivateConfig: NodePrivateConfig,
) extends NodeConfig.Section {
    override transparent inline def nodeConfig: NodeConfig = this
}

object NodeConfig {
    trait Section extends NodePrivateConfig.Section, HeadConfig.Section {
        def nodeConfig: NodeConfig

        override transparent inline def cardanoNetwork: CardanoNetwork =
            headConfig.cardanoNetwork
        override transparent inline def headParams: HeadParameters =
            headConfig.headParams
        override transparent inline def headPeers: HeadPeers =
            headConfig.headPeers
        override transparent inline def initialBlock: Block.MultiSigned.Initial =
            headConfig.initialBlock
        override transparent inline def initializationParams: InitializationParameters =
            headConfig.initializationParams

        override transparent inline def ownHeadPeer: OwnHeadPeer = nodePrivateConfig.ownHeadPeer
        override transparent inline def nodeOperationLiquidationConfig
            : NodeOperationLiquidationConfig =
            nodePrivateConfig.nodeOperationLiquidationConfig
        override transparent inline def nodeOperationMultisigConfig: NodeOperationMultisigConfig =
            nodePrivateConfig.nodeOperationMultisigConfig
    }
}
