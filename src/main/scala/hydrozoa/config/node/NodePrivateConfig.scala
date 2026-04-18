package hydrozoa.config.node

import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.node.operation.evacuation.NodeOperationEvacuationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate

final case class NodePrivateConfig(
    override val ownHeadPeerPrivate: OwnHeadPeerPrivate,
    override val nodeOperationEvacuationConfig: NodeOperationEvacuationConfig,
    override val nodeOperationMultisigConfig: NodeOperationMultisigConfig,
    // Adding this here because it's not something that the peers necessarily need to agree upon.
    override val scriptReferenceUtxos: ScriptReferenceUtxos,
) extends NodePrivateConfig.Section {
    override transparent inline def nodePrivateConfig: NodePrivateConfig = this
}

object NodePrivateConfig {
    trait Section
        extends NodeOperationMultisigConfig.Section,
          NodeOperationEvacuationConfig.Section,
          OwnHeadPeerPrivate.Section,
          ScriptReferenceUtxos.Section {
        def nodePrivateConfig: NodePrivateConfig

        def nodeOperationEvacuationConfig: NodeOperationEvacuationConfig =
            nodePrivateConfig.nodeOperationEvacuationConfig

        override def nodeOperationMultisigConfig: NodeOperationMultisigConfig =
            nodePrivateConfig.nodeOperationMultisigConfig

        override def ownHeadPeerPrivate: OwnHeadPeerPrivate = nodePrivateConfig.ownHeadPeerPrivate

        override def scriptReferenceUtxos: ScriptReferenceUtxos =
            nodePrivateConfig.scriptReferenceUtxos
    }
}
