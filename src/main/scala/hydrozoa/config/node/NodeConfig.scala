package hydrozoa.config.node

import cats.data.EitherT
import cats.effect.*
import cats.syntax.contravariant.*
import hydrozoa.lib.StartupRefusal
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
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, warn}
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost, CardanoBackendEventFormat}
import hydrozoa.multisig.consensus.peer.PeerId.isCoil
import hydrozoa.multisig.consensus.peer.PeerWallet
import io.circe.{parser, *}
import java.nio.file.{Files, Path}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

final case class NodeConfig private (
    override val headConfig: HeadConfig,
    override val nodePrivateConfig: NodePrivateConfig,
) extends NodeConfig.Section {
    override transparent inline def nodeConfig: NodeConfig = this

    // Safety invariant (head peers only): a head peer's CardanoLiaison must poll at least
    // `cardanoLiaisonPollingPeriodSafetyFactor` times within `depositMaturityDuration`, so that
    // by the time a deposit is mature every head peer has observed it on L1. Otherwise the leader
    // (which has seen the deposit) and a head follower (which hasn't yet) disagree on whether to
    // absorb or refund, breaking consensus. Coil peers are exempt: they classify deposit existence
    // from the head peers' soft-confirmed view rather than their own polls (see
    // DepositsMap.Existence), so their poll cadence is unconstrained and can be set far slower.
    private val pollingPeriod =
        nodePrivateConfig.nodeOperationMultisigConfig.cardanoLiaisonPollingPeriod
    private val maxPollingPeriod = headConfig.maxCardanoLiaisonPollingPeriod
    require(
      nodePrivateConfig.ownPeerId.isCoil || pollingPeriod <= maxPollingPeriod,
      s"cardanoLiaisonPollingPeriod ($pollingPeriod) exceeds the maximum allowed for this " +
          s"head's depositMaturityDuration (${headConfig.depositMaturityDuration}): " +
          s"$maxPollingPeriod (= depositMaturityDuration / " +
          s"${hydrozoa.config.head.multisig.timing.TxTiming.cardanoLiaisonPollingPeriodSafetyFactor})"
    )
}

object NodeConfig {

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("hydrozoa"))

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
                given CoilPeers = coilPeers
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
        remoteLedgerUri: Option[String],
        remoteScreenerUri: Option[String] = None,
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
          remoteLedgerUri,
          remoteScreenerUri,
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
        remoteLedgerUri: Option[String],
        remoteScreenerUri: Option[String] = None,
        adminUsername: String,
        adminPassword: String,
        httpHost: String,
        httpPort: String,
    ): Option[NodeConfig] = for {
        ownCoilPeerPrivate <- OwnCoilPeerPrivate(ownCoilWallet, headConfig.coilPeers)
        nodePrivateConfig = NodePrivateConfig(
          ownCoilPeerPrivate,
          nodeOperationEvacuationConfig,
          nodeOperationMultisigConfig,
          blockfrostApiKey,
          remoteLedgerUri,
          remoteScreenerUri,
          adminUsername,
          adminPassword,
          httpHost,
          httpPort,
        )
    } yield NodeConfig(headConfig, nodePrivateConfig)

    /** Read both config files and decode the resulting [[NodeConfig]] together with the Blockfrost
      * backend the decoder constructs. Shared by every CLI that needs to act as a configured peer
      * ([[hydrozoa.app.Main]], [[hydrozoa.bootstrap.Migrate]]).
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
            privateRaw <- IO.blocking(Files.readString(privateConfigPath))
            // Credentials come from the environment, not from the config file, so they are spliced
            // in before decoding and the decoders below are unchanged. A missing or mismatched one
            // is a `StartupRefusal` raised here — deliberately ahead of the backend reads, since it
            // is a verdict about this node alone and no amount of retrying changes it.
            privateJson <- IO.fromEither(
              io.circe.parser
                  .parse(privateRaw)
                  .left
                  .map(e =>
                      StartupRefusal(s"$privateConfigPath is not valid JSON: ${e.getMessage}")
                  )
            )
            privateStr <- PrivateSecrets.overlay(privateConfigPath, privateJson).map(_.noSpaces)
            loaded <- retryingBackendReads(
              backendReadRetryWaits,
              NodeConfig.fromJson(headStr, privateStr, backendOverride).value
            ).flatMap {
                case Left(err) =>
                    // Reached only for a DETERMINISTIC failure: `isWorthRetrying` retries a
                    // backend error forever, so anything arriving here is a malformed config or a
                    // script reference UTxO that is not on chain. Re-deriving that verdict on a
                    // supervisor restart loop teaches nobody anything.
                    IO.raiseError(
                      StartupRefusal(s"failed to load NodeConfig: $err")
                    )
                case Right(ok) => IO.pure(ok)
            }
        } yield loaded

    /** Waits before each retry of the config load's backend reads.
      *
      * Decoding a config resolves the script reference UTxOs against Cardano, so loading it is a
      * network operation, and a node that cannot reach the backend for a few seconds exits — which
      * a process supervisor answers by starting it again, immediately, into the same failure. That
      * turns a brief loss of egress into a crash loop that outlives it.
      *
      * ⛔ UNBOUNDED, deliberately, and this is the whole point. The ladder used to stop after ~112
      * seconds, which meant any loss of egress longer than two minutes ended in exactly the crash
      * loop the paragraph above set out to prevent. A node that cannot reach Cardano cannot do
      * anything useful, so exiting buys nothing and costs an endless restart cycle; waiting is
      * strictly better and is visible on the dashboard.
      *
      * ⚠️ This does NOT make a wrong config wait: [[isWorthRetrying]] retries only
      * `CardanoBackendError`. A malformed config or an unresolvable script UTxO still fails
      * immediately, because re-reading the same bytes gives the same answer. That split is the
      * difference between "the world is not ready yet" and "this node is misconfigured", and the
      * two must never be treated alike.
      *
      * It backs off 2s, 5s, 15s, 30s and then every 60s forever — capped so a node that has been
      * waiting for hours still reacts promptly when egress returns.
      */
    private val backendReadRetryWaits: LazyList[FiniteDuration] =
        LazyList(2.seconds, 5.seconds, 15.seconds, 30.seconds) #::: LazyList.continually(60.seconds)

    /** Whether a failed load is worth another attempt. Anything that reached the Cardano backend is
      * — including an error the backend classified, since a DNS failure surfaces as a resolve error
      * rather than a raised exception. A malformed config is not: re-reading the same bytes gives
      * the same answer.
      */
    private def isWorthRetrying(
        err: ScriptReferenceUtxos.Error | io.circe.Error
    ): Boolean = err match
        case _: ScriptReferenceUtxos.Error.CardanoBackendError => true
        case _                                                 => false

    /** Re-run `attempt` while it fails for a reason another attempt could fix, waiting `waits` in
      * turn and giving up with the last failure once they run out. `waits` is a parameter so a test
      * can drive the same loop without waiting minutes.
      */
    private[node] def retryingBackendReads[A](
        waits: LazyList[FiniteDuration],
        attempt: IO[Either[ScriptReferenceUtxos.Error | io.circe.Error, A]]
    ): IO[Either[ScriptReferenceUtxos.Error | io.circe.Error, A]] =
        def go(
            remaining: LazyList[FiniteDuration]
        ): IO[Either[ScriptReferenceUtxos.Error | io.circe.Error, A]] =
            // A raised throwable gets the same treatment as a backend error: building the backend
            // is itself a network operation, and it reports failure by raising rather than by a
            // Left.
            attempt.attempt.flatMap {
                case Right(Right(ok)) => IO.pure(Right(ok))
                case other =>
                    val reason = other match
                        case Left(t)         => t.toString
                        case Right(Left(e))  => e.toString
                        case Right(Right(_)) => ""
                    val retryable = other match
                        case Left(_)        => true
                        case Right(Left(e)) => isWorthRetrying(e)
                        case _              => false
                    (remaining, retryable) match
                        case (wait #:: rest, true) =>
                            // ⛔ Do NOT report "attempts left" here: `remaining` is an
                            // infinite LazyList in production and forcing its length hangs.
                            log.warn(
                              s"Config load failed against the Cardano backend ($reason); " +
                                  s"retrying in $wait. The node cannot start without this, so it " +
                                  "WAITS rather than exiting into a restart loop."
                            ) >> IO.sleep(wait) >> go(rest)
                        case _ =>
                            other match
                                case Left(t)     => IO.raiseError(t)
                                case Right(left) => IO.pure(left)
            }
        go(waits)

    trait Section extends NodePrivateConfig.Section, HeadConfig.Section {
        def nodeConfig: NodeConfig

        def headConfig: HeadConfig = nodeConfig.headConfig
        def nodePrivateConfig: NodePrivateConfig = nodeConfig.nodePrivateConfig
    }
}
