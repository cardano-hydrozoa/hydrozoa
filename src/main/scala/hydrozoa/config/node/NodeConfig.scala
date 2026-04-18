package hydrozoa.config.node

import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.node.operation.evacuation.NodeOperationEvacuationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.multisig.consensus.peer.HeadPeerWallet

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
        nodeOperationEvacuationConfig: NodeOperationEvacuationConfig,
        nodeOperationMultisigConfig: NodeOperationMultisigConfig,
        scriptReferenceUtxos: ScriptReferenceUtxos
    ): Option[NodeConfig] = for {
        ownHeadPeerPrivate <- OwnHeadPeerPrivate(ownHeadWallet, headConfig.headPeers)
        nodePrivateConfig = NodePrivateConfig(
          ownHeadPeerPrivate,
          nodeOperationEvacuationConfig,
          nodeOperationMultisigConfig,
          scriptReferenceUtxos
        )
    } yield NodeConfig(headConfig, nodePrivateConfig)

    trait Section extends NodePrivateConfig.Section, HeadConfig.Section {
        def nodeConfig: NodeConfig

        def headConfig: HeadConfig = nodeConfig.headConfig
        def nodePrivateConfig: NodePrivateConfig = nodeConfig.nodePrivateConfig
    }
}
