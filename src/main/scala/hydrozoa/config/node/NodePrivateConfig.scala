package hydrozoa.config.node

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.operation.evacuation.NodeOperationEvacuationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import io.circe.*
import io.circe.generic.semiauto.*

final case class NodePrivateConfig(
    override val ownHeadPeerPrivate: OwnHeadPeerPrivate,
    override val nodeOperationEvacuationConfig: NodeOperationEvacuationConfig,
    override val nodeOperationMultisigConfig: NodeOperationMultisigConfig,
    override val hydrozoaHost: String,
    override val hydrozoaPort: String,
    override val blockfrostApiKey: String,
) extends NodePrivateConfig.Section {
    override transparent inline def nodePrivateConfig: NodePrivateConfig = this
}

object NodePrivateConfig {
    trait Section
        extends NodeOperationMultisigConfig.Section,
          NodeOperationEvacuationConfig.Section,
          OwnHeadPeerPrivate.Section {
        def nodePrivateConfig: NodePrivateConfig

        def ownHeadPeerPrivate: OwnHeadPeerPrivate = nodePrivateConfig.ownHeadPeerPrivate

        def nodeOperationEvacuationConfig: NodeOperationEvacuationConfig =
            nodePrivateConfig.nodeOperationEvacuationConfig

        def nodeOperationMultisigConfig: NodeOperationMultisigConfig =
            nodePrivateConfig.nodeOperationMultisigConfig

        def hydrozoaHost: String = nodePrivateConfig.hydrozoaHost

        def hydrozoaPort: String = nodePrivateConfig.hydrozoaPort

        def blockfrostApiKey: String = nodePrivateConfig.blockfrostApiKey
    }

    given nodePrivateConfigEncoder: Encoder[NodePrivateConfig] =
        deriveEncoder[NodePrivateConfig]

    given nodePrivateConfigDecoder(using
        headPeers: HeadPeers.Section,
        network: CardanoNetwork.Section
    ): Decoder[NodePrivateConfig] = deriveDecoder[NodePrivateConfig]
}
