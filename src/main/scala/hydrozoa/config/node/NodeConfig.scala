package hydrozoa.config.node

import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.HeadParameters
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.operation.liquidation.NodeOperationLiquidationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.multisig.consensus.peer.HeadPeerWallet
import hydrozoa.multisig.ledger.block.Block

final case class NodeConfig private (
    override val headConfig: HeadConfig,
    override val nodePrivateConfig: NodePrivateConfig,
) extends NodeConfig.Section {
    override transparent inline def nodeConfig: NodeConfig = this
}

object NodeConfig {
    def apply(
        headConfig: HeadConfig,
        ownHeadWallet: HeadPeerWallet,
        nodeOperationLiquidationConfig: NodeOperationLiquidationConfig,
        nodeOperationMultisigConfig: NodeOperationMultisigConfig
    ): Option[NodeConfig] = for {
        ownHeadPeerPrivate <- OwnHeadPeerPrivate(ownHeadWallet, headConfig.headPeers)
        nodePrivateConfig = NodePrivateConfig(
          ownHeadPeerPrivate,
          nodeOperationLiquidationConfig,
          nodeOperationMultisigConfig
        )
    } yield NodeConfig(headConfig, nodePrivateConfig)

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

        override transparent inline def ownHeadPeerPrivate: OwnHeadPeerPrivate =
            nodePrivateConfig.ownHeadPeerPrivate
        override transparent inline def nodeOperationLiquidationConfig
            : NodeOperationLiquidationConfig =
            nodePrivateConfig.nodeOperationLiquidationConfig
        override transparent inline def nodeOperationMultisigConfig: NodeOperationMultisigConfig =
            nodePrivateConfig.nodeOperationMultisigConfig
    }
}
