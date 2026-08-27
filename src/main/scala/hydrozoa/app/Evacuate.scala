package hydrozoa.app

import cats.effect.{ExitCode, IO, Resource}
import cats.syntax.apply.*
import cats.syntax.contravariant.*
import com.monovore.decline.{Command, Opts}
import com.suprnation.actor.ActorSystem
import hydrozoa.BuildInfo
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info}
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.CardanoLiaisonEventFormat
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.persistence.rocksdb.RocksDbBackendStore
import hydrozoa.multisig.persistence.{Cf, Persistence, PersistenceEventFormat, StoreIdentity}
import hydrozoa.rulebased.{RuleBasedActorEventFormat, RuleBasedRegimeManager}
import java.nio.file.Path

/** The `evacuate` subcommand of the `hydrozoa` CLI: start a node **directly in the rule-based
  * regime** from a database left by a previous head run, plus the same head + private config
  * `serve` takes (design `docs/spec/evacuate-command.md`).
  *
  * Usage:
  * {{{
  *   hydrozoa evacuate <head-config.json> <peer-private.json>
  * }}}
  *
  * It boots only the L1 boundary — a [[hydrozoa.rulebased.RuleBasedRegimeManager]] with its own
  * `CardanoLiaison` + `RuleBasedActor`, no consensus/mesh/HTTP/L2 machinery. `CardanoLiaison`
  * submits the fallback (driving the head into the rule-based regime if it is not there yet) and
  * finishes any in-flight rollouts; `RuleBasedActor` runs the dispute → evacuation. The persistence
  * store is opened **read-only**: neither actor writes to it in this mode. The command runs
  * resident — it keeps polling L1 (a rollback can re-introduce work at any time) until the operator
  * stops it.
  */
object Evacuate {

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("hydrozoa.app.Evacuate"))

    private val headConfigPathArg: Opts[Path] =
        Opts.argument[String]("head-config.json").map(Path.of(_))

    private val privateConfigPathArg: Opts[Path] =
        Opts.argument[String]("peer-private.json").map(Path.of(_))

    /** Parent dir of the per-peer RocksDB store, opened read-only. Defaults to `.hydrozoa-data` (as
      * `serve`); surfaced as a flag so `evacuate` can be pointed at a stopped head's data dir
      * without matching its working directory.
      */
    private val dataDirOpt: Opts[Path] =
        Opts
            .option[String](
              "data-dir",
              "Parent dir of the per-peer persistence store (default: .hydrozoa-data)"
            )
            .map(Path.of(_))
            .withDefault(Path.of(".hydrozoa-data"))

    /** The `evacuate` subcommand: run the rule-based regime standalone from a generated config. */
    lazy val command: Command[IO[ExitCode]] =
        Command(
          name = "evacuate",
          header = "Run the rule-based regime (dispute + evacuation) from a left-over database"
        )(
          (headConfigPathArg, privateConfigPathArg, dataDirOpt).mapN((h, p, d) =>
              runEvacuation(h, p, d)
          )
        )

    /** Run the rule-based regime standalone from a loaded config.
      *
      * @param dataDir
      *   filesystem location of the per-peer RocksDB persistence store, opened read-only; the store
      *   path is `${dataDir}/peer-${ownPeerLabel}/rocksdb`. Defaults to `.hydrozoa-data` (as
      *   `serve`), so pointing `evacuate` at a stopped head's data dir just works.
      * @param backendOverride
      *   if `Some`, used in place of the Blockfrost backend the decoder would build. Lets tests
      *   inject a mock.
      */
    def runEvacuation(
        headConfigPath: Path,
        privateConfigPath: Path,
        dataDir: Path = Path.of(".hydrozoa-data"),
        backendOverride: Option[CardanoBackend[IO]] = None,
    ): IO[ExitCode] = {
        val setupIO = for {
            _ <- log.info(
              s"Hydrozoa ${BuildInfo.version} " +
                  s"(git ${BuildInfo.gitCommit}, built ${BuildInfo.builtAtString})"
            )
            _ <- log.info("Starting Hydrozoa evacuation (rule-based regime)...")
            _ <- log.info(s"Loading head config from $headConfigPath")
            _ <- log.info(s"Loading peer private config from $privateConfigPath")
            loaded <- NodeConfig.load(headConfigPath, privateConfigPath, backendOverride)
            (nodeConfig, backend) = loaded
            _ <- log.info(
              s"Entering evacuate mode for head ${nodeConfig.headId.bytes.toHex} " +
                  s"(peer ${nodeConfig.ownPeerLabel})"
            )
            _ <- log.info(s"headAddress: ${nodeConfig.headMultisigAddress.toBech32.get}")
        } yield (backend, nodeConfig)

        val resource = for {
            result <- Resource.eval(setupIO)
            (backend, nodeConfig) = result

            // Synthetic peer label for log lines: head peers use their own number; coil peers are
            // offset past the head set so their lines stay distinguishable (as in `Serve`).
            labelNum = nodeConfig.ownPeerId match {
                case PeerId.Head(n) => n
                case PeerId.Coil(c) =>
                    HeadPeerNumber(nodeConfig.headConfig.headPeerNums.size + c.convert)
            }

            persistenceTracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
            // Read-only: the rule-based regime only reads persistence (design
            // `docs/spec/evacuate-command.md`), so RO is both correct and safer — no exclusive lock,
            // and a stray write would throw.
            backendStore <- RocksDbBackendStore.openReadOnly(
              dataDir.resolve(s"peer-${nodeConfig.ownPeerLabel}/rocksdb"),
              Cf.mkAll(
                headPeers = nodeConfig.headConfig.headPeerNums.toList,
                coilPeers = nodeConfig.headConfig.coilPeers.coilPeerNumbers,
                hubs = nodeConfig.headConfig.coilPeers.hubHeadPeerNumbers
              ),
              StoreIdentity(
                headParamsHash = nodeConfig.headParamsHash,
                headId = nodeConfig.headId,
                ownPeerId = nodeConfig.ownPeerId
              ),
              persistenceTracer,
            )
            persistence <- Resource.eval {
                given CardanoNetwork.Section = nodeConfig
                // Read-only: the store was opened read-only, so this must not bump the arrival-stamp
                // generation or record a zero-time (both are writes). The rule-based regime only
                // reads (design `docs/spec/evacuate-command.md`).
                Persistence.fromBackendReadOnly(backendStore, persistenceTracer)
            }

            system <- ActorSystem[IO]("Hydrozoa Evacuate")
        } yield (backend, nodeConfig, labelNum, persistence, system)

        resource.use { case (backend, nodeConfig, labelNum, persistence, system) =>
            val ruleBasedTracer =
                Slf4jTracer.sink.contramap(RuleBasedActorEventFormat.humanFormat(labelNum))
            val cardanoLiaisonTracer =
                Slf4jTracer.sink.contramap(CardanoLiaisonEventFormat.humanFormat(labelNum))
            for {
                _ <- system.actorOf(
                  RuleBasedRegimeManager(
                    cardanoBackend = backend,
                    persistence = persistence,
                    tracer = ruleBasedTracer,
                    ownLiaison = Some(RuleBasedRegimeManager.OwnLiaison(cardanoLiaisonTracer)),
                  )(using nodeConfig),
                  "RuleBasedRegimeManager"
                )
                _ <- log.info("Evacuation node started; polling L1 for rule-based utxos")
                _ <- system.waitForTermination
            } yield ExitCode.Success
        }
    }
}
