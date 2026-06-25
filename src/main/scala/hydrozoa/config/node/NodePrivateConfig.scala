package hydrozoa.config.node

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.operation.evacuation.NodeOperationEvacuationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.OwnPeerPrivate
import io.circe.*
import io.circe.generic.semiauto.*

final case class NodePrivateConfig(
    override val ownPeerPrivate: OwnPeerPrivate,
    override val nodeOperationEvacuationConfig: NodeOperationEvacuationConfig,
    override val nodeOperationMultisigConfig: NodeOperationMultisigConfig,
    override val blockfrostApiKey: String,
    override val sugarRushUri: String,
    override val adminUsername: String,
    override val adminPassword: String,
    override val httpHost: String,
    override val httpPort: String,
) extends NodePrivateConfig.Section {
    override transparent inline def nodePrivateConfig: NodePrivateConfig = this
}

object NodePrivateConfig {
    trait Section
        extends NodeOperationMultisigConfig.Section,
          NodeOperationEvacuationConfig.Section,
          OwnPeerPrivate.Section {
        def nodePrivateConfig: NodePrivateConfig

        def ownPeerPrivate: OwnPeerPrivate = nodePrivateConfig.ownPeerPrivate

        def nodeOperationEvacuationConfig: NodeOperationEvacuationConfig =
            nodePrivateConfig.nodeOperationEvacuationConfig

        def nodeOperationMultisigConfig: NodeOperationMultisigConfig =
            nodePrivateConfig.nodeOperationMultisigConfig

        def blockfrostApiKey: String = nodePrivateConfig.blockfrostApiKey

        def sugarRushUri: String = nodePrivateConfig.sugarRushUri

        def adminUsername: String = nodePrivateConfig.adminUsername

        def adminPassword: String = nodePrivateConfig.adminPassword

        def httpHost: String = nodePrivateConfig.httpHost

        def httpPort: String = nodePrivateConfig.httpPort
    }

    given nodePrivateConfigEncoder: Encoder[NodePrivateConfig] =
        deriveEncoder[NodePrivateConfig]

    given nodePrivateConfigDecoder(using
        headPeers: HeadPeers.Section,
        network: CardanoNetwork.Section
    ): Decoder[NodePrivateConfig] = deriveDecoder[NodePrivateConfig]
}
