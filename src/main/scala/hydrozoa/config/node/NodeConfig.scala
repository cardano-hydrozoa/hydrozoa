package hydrozoa.config.node

import cats.data.EitherT
import cats.effect.*
import cats.syntax.contravariant.*
import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.coil.CoilPeers
import hydrozoa.config.head.coil.CoilPeers.coilPeersDecoder
import hydrozoa.config.head.network.CardanoNetwork.{Custom, cardanoNetworkDecoder}
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.head.peers.HeadPeers.headPeersDecoder
import hydrozoa.config.node.NodePrivateConfig.given
import hydrozoa.config.node.operation.evacuation.NodeOperationEvacuationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.{OwnCoilPeerPrivate, OwnHeadPeerPrivate}
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost, CardanoBackendEventFormat}
import hydrozoa.multisig.consensus.peer.PeerWallet
import io.circe.{parser, *}
import java.nio.file.{Files, Path}
import scalus.crypto.ed25519.VerificationKey

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
        nodePrivateConfigStr: String,
        backendOverride: Option[CardanoBackend[IO]] = None,
    ): EitherT[IO, ScriptReferenceUtxos.Error | io.circe.Error, (NodeConfig, CardanoBackend[IO])] =
        for {
            network <- EitherT.fromEither[IO] {
                given onlyNetwork: Decoder[CardanoNetwork] = Decoder.instance(c =>
                    c.downField("cardanoNetwork")
                        .as[CardanoNetwork](using cardanoNetworkDecoder)
                )
                parser.decode(headConfigStr)
            }
            headPeers <- EitherT.fromEither[IO] {
                given onlyHeadPeers: Decoder[HeadPeers] = Decoder.instance(c =>
                    c.downField("headPeers")
                        .as[HeadPeers](using headPeersDecoder)
                )
                parser.decode(headConfigStr)
            }
            coilPeers <- EitherT.fromEither[IO] {
                given onlyCoilPeers: Decoder[CoilPeers] =
                    Decoder.instance(c =>
                        c.downField("coilPeers").as[CoilPeers](using coilPeersDecoder)
                    )
                parser.decode(headConfigStr)
            }

            privateConfig <- EitherT.fromEither[IO] {
                given HeadPeers = headPeers
                given List[VerificationKey] = coilPeers.verificationKeys
                given CardanoNetwork = network
                io.circe.parser.decode(nodePrivateConfigStr)(using nodePrivateConfigDecoder)
            }

            // Use the caller-provided backend (e.g., a mock from a test) when supplied; otherwise
            // build a real Blockfrost backend from the private config's API key + the network.
            cardanoBackend <- backendOverride match {
                case Some(b) => EitherT.pure[IO, ScriptReferenceUtxos.Error | io.circe.Error](b)
                case None =>
                    val blockfrostNetwork = network match {
                        case n: StandardCardanoNetwork => Left(n)
                        // TODO: need a blockfrost url here
                        case custom: Custom => Right((custom, ??? : CardanoBackendBlockfrost.URL))
                    }
                    EitherT.liftF(
                      CardanoBackendBlockfrost(
                        blockfrostNetwork,
                        privateConfig.blockfrostApiKey,
                        tracer = Slf4jTracer.sink.contramap(CardanoBackendEventFormat.humanFormat)
                      )
                    )
            }

            headConfig <- HeadConfig.fromJson(headConfigStr, cardanoBackend)

        } yield (NodeConfig(headConfig, privateConfig), cardanoBackend)

    /** Build a head node's config: the shared `headConfig` plus the private identity layer carrying
      * an [[OwnHeadPeerPrivate]] (this head's wallet + its derived `HeadPeerNumber`). `None` if the
      * wallet's key is not among the configured head peers.
      */
    def mkHeadConfig(
        headConfig: HeadConfig,
        ownHeadWallet: PeerWallet,
        nodeOperationEvacuationConfig: NodeOperationEvacuationConfig,
        nodeOperationMultisigConfig: NodeOperationMultisigConfig,
        blockfrostApiKey: String,
        sugarRushUri: String,
        adminUsername: String,
        adminPassword: String,
        httpHost: String,
        httpPort: String,
    ): Option[NodeConfig] = for {
        ownHeadPeerPrivate <- OwnHeadPeerPrivate(ownHeadWallet, headConfig.headPeers)
        nodePrivateConfig = NodePrivateConfig(
          ownHeadPeerPrivate,
          nodeOperationEvacuationConfig,
          nodeOperationMultisigConfig,
          blockfrostApiKey,
          sugarRushUri,
          adminUsername,
          adminPassword,
          httpHost,
          httpPort,
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
        blockfrostApiKey: String,
        sugarRushUri: String,
        adminUsername: String,
        adminPassword: String,
        httpHost: String,
        httpPort: String,
    ): Option[NodeConfig] = for {
        ownCoilPeerPrivate <- OwnCoilPeerPrivate(ownCoilWallet, headConfig.coilPeerVKeys)
        nodePrivateConfig = NodePrivateConfig(
          ownCoilPeerPrivate,
          nodeOperationEvacuationConfig,
          nodeOperationMultisigConfig,
          blockfrostApiKey,
          sugarRushUri,
          adminUsername,
          adminPassword,
          httpHost,
          httpPort,
        )
    } yield NodeConfig(headConfig, nodePrivateConfig)

    /** Read both config files and decode the resulting [[NodeConfig]] together with the Blockfrost
      * backend the decoder constructs. Shared by every CLI that needs to act as a configured peer
      * ([[hydrozoa.app.Main]], [[hydrozoa.app.Migrate]]).
      *
      * @param backendOverride
      *   if `Some`, used in place of the Blockfrost backend the decoder would otherwise build from
      *   the private config's API key. Tests pass a mock; CLIs leave it `None`.
      */
    def load(
        headConfigPath: Path,
        privateConfigPath: Path,
        backendOverride: Option[CardanoBackend[IO]] = None,
    ): IO[(NodeConfig, CardanoBackend[IO])] =
        for {
            headStr <- IO.blocking(Files.readString(headConfigPath))
            privateStr <- IO.blocking(Files.readString(privateConfigPath))
            loaded <- NodeConfig
                .fromJson(headStr, privateStr, backendOverride)
                .foldF(
                  err => IO.raiseError(new RuntimeException(s"Failed to load NodeConfig: $err")),
                  IO.pure
                )
        } yield loaded

    trait Section extends NodePrivateConfig.Section, HeadConfig.Section {
        def nodeConfig: NodeConfig

        def headConfig: HeadConfig = nodeConfig.headConfig
        def nodePrivateConfig: NodePrivateConfig = nodeConfig.nodePrivateConfig
    }
}
