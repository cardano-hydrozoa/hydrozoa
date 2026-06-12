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
import hydrozoa.config.node.owninfo.{OwnCoilPeerPrivate, OwnHeadPeerPrivate}
import hydrozoa.multisig.backend.cardano.CardanoBackendBlockfrost
import hydrozoa.multisig.consensus.peer.PeerWallet
import io.circe.{parser, *}

final case class NodeConfig private (
    override val headConfig: HeadConfig,
    override val nodePrivateConfig: NodePrivateConfig,
) extends NodeConfig.Section {
    override transparent inline def nodeConfig: NodeConfig = this

    // Safety invariant: a peer's CardanoLiaison must poll at least
    // `cardanoLiaisonPollingPeriodSafetyFactor` times within `depositMaturityDuration`,
    // so that by the time a deposit is mature every peer has observed it on L1. Otherwise
    // the leader (which has seen the deposit) and a follower (which hasn't yet) will disagree
    // on whether to absorb or refund, breaking consensus.
    private val pollingPeriod =
        nodePrivateConfig.nodeOperationMultisigConfig.cardanoLiaisonPollingPeriod
    private val maxPollingPeriod = headConfig.maxCardanoLiaisonPollingPeriod
    require(
      pollingPeriod <= maxPollingPeriod,
      s"cardanoLiaisonPollingPeriod ($pollingPeriod) exceeds the maximum allowed for this " +
          s"head's depositMaturityDuration (${headConfig.depositMaturityDuration}): " +
          s"$maxPollingPeriod (= depositMaturityDuration / " +
          s"${hydrozoa.config.head.multisig.timing.TxTiming.cardanoLiaisonPollingPeriodSafetyFactor})"
    )
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

    /** Build a head node's config: the shared `headConfig` plus the private identity layer carrying
      * an [[OwnHeadPeerPrivate]] (this head's wallet + its derived `HeadPeerNumber`). `None` if the
      * wallet's key is not among the configured head peers.
      */
    def mkHeadConfig(
        headConfig: HeadConfig,
        ownHeadWallet: PeerWallet,
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

    /** Build a coil node's config: the same shared `headConfig` a head peer gets, with the private
      * identity layer carrying an [[OwnCoilPeerPrivate]] (this coil's wallet + its derived
      * `CoilPeerNumber`). `None` if the wallet's key is not among the configured coil peers.
      */
    def mkCoilConfig(
        headConfig: HeadConfig,
        ownCoilWallet: PeerWallet,
        nodeOperationEvacuationConfig: NodeOperationEvacuationConfig,
        nodeOperationMultisigConfig: NodeOperationMultisigConfig,
        hydrozoaHost: String,
        hydrozoaPort: String,
        blockfrostApiKey: String
    ): Option[NodeConfig] = for {
        ownCoilPeerPrivate <- OwnCoilPeerPrivate(ownCoilWallet, headConfig.coilPeerVKeys)
        nodePrivateConfig = NodePrivateConfig(
          ownCoilPeerPrivate,
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
