package hydrozoa.config.node

import cats.data.EitherT
import cats.effect.*
import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork.{Custom, cardanoNetworkDecoder}
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.head.peers.HeadPeers.headPeersDecoder
import hydrozoa.config.node.NodePrivateConfig.given
import hydrozoa.config.node.operation.evacuation.NodeOperationEvacuationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.multisig.backend.cardano.CardanoBackendBlockfrost
import hydrozoa.multisig.consensus.peer.HeadPeerWallet
import io.circe.{parser, *}

final case class NodeConfig private (
    override val headConfig: HeadConfig,
    override val nodePrivateConfig: NodePrivateConfig,
) extends NodeConfig.Section {
    override transparent inline def nodeConfig: NodeConfig = this
}

object NodeConfig {

    def fromJson(
        headConfigStr: String,
        nodePrivateConfigStr: String
    ): EitherT[IO, ScriptReferenceUtxos.Error | io.circe.Error, NodeConfig] =
        for {
            network <- EitherT.fromEither[IO] {
                given onlyNetwork: Decoder[CardanoNetwork] = Decoder.instance(c =>
                    c.downField("headConfigBootstrap")
                        .downField("cardanoNetwork")
                        .as[CardanoNetwork](using cardanoNetworkDecoder)
                )
                parser.decode(headConfigStr)
            }
            headPeers <- EitherT.fromEither[IO] {
                given onlyHeadPeers: Decoder[HeadPeers] = Decoder.instance(c =>
                    c.downField("headConfigBootstrap")
                        .downField("headPeers")
                        .as[HeadPeers](using headPeersDecoder)
                )
                parser.decode(headConfigStr)
            }

            privateConfig <- EitherT.fromEither[IO] {
                given HeadPeers = headPeers
                given CardanoNetwork = network
                io.circe.parser.decode(nodePrivateConfigStr)(using nodePrivateConfigDecoder)
            }

            blockfrostNetwork = network match {
                case n: StandardCardanoNetwork => Left(n)
                // TODO: need a blockfrost url here
                case custom: Custom => Right((custom, ??? : CardanoBackendBlockfrost.URL))
            }

            cardanoBackend <- EitherT.liftF(
              CardanoBackendBlockfrost(blockfrostNetwork, privateConfig.blockfrostApiKey)
            )

            headConfig <- HeadConfig.fromJson(headConfigStr, cardanoBackend)

        } yield NodeConfig(headConfig, privateConfig)

    def apply(
        headConfig: HeadConfig,
        ownHeadWallet: HeadPeerWallet,
        nodeOperationEvacuationConfig: NodeOperationEvacuationConfig,
        nodeOperationMultisigConfig: NodeOperationMultisigConfig,
        hydrozoaHost: String,
        hydrozoaPort: String,
        blockfrostApiKey: String
    ): Option[NodeConfig] = for {
        ownHeadPeerPrivate <- OwnHeadPeerPrivate(ownHeadWallet, headConfig.headPeers)
        nodePrivateConfig = NodePrivateConfig(
          ownHeadPeerPrivate,
          nodeOperationEvacuationConfig,
          nodeOperationMultisigConfig,
          hydrozoaHost,
          hydrozoaPort,
          blockfrostApiKey
        )
    } yield NodeConfig(headConfig, nodePrivateConfig)

    trait Section extends NodePrivateConfig.Section, HeadConfig.Section {
        def nodeConfig: NodeConfig

        def headConfig: HeadConfig = nodeConfig.headConfig
        def nodePrivateConfig: NodePrivateConfig = nodeConfig.nodePrivateConfig
    }
}
