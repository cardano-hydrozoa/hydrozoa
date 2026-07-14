package hydrozoa.bootstrap

import cats.data.{NonEmptyList, NonEmptyMap, Validated}
import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import com.bloxbean.cardano.client.util.HexUtil
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.coil.{CoilPeerData, CoilPeers}
import hydrozoa.config.head.initialization.{InitialBlock, InitializationParameters}
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.fallback.FallbackContingency.mkFallbackContingencyWithDefaults
import hydrozoa.config.head.multisig.settlement.SettlementConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.config.head.parameters.{HeadParameters, L2LedgerKind}
import hydrozoa.config.head.peers.{HeadPeerData, HeadPeers}
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.logging.{ContraTracer, Logging, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info, warn}
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost, CardanoBackendEventFormat}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.peer.HeadPeerNumber.given
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockHeader}
import hydrozoa.multisig.ledger.eutxol2.toEvacuationMap
import hydrozoa.multisig.ledger.eutxol2.tx.L2Genesis
import hydrozoa.multisig.ledger.l1.tx.RawTx
import hydrozoa.multisig.ledger.l1.txseq.InitializationTxSeq
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json, parser}
import java.nio.file.{Files, Path}
import java.security.SecureRandom
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.params.{Ed25519KeyGenerationParameters, Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}
import org.http4s.Uri
import scala.collection.immutable.SortedMap
import scala.util.Try
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Coin, EvaluatorMode, Hash32, PlutusScriptEvaluator, TransactionInput, TransactionOutput, Utxo, Utxos, Value}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}
import scalus.cardano.txbuilder.{Change, TransactionBuilder}
import scalus.crypto.ed25519.{SigningKey, VerificationKey}
import scalus.uplc.builtin.ByteString

object Bootstrap:

    private val logger = Logging.loggerIO("hydrozoa.bootstrap.Bootstrap")

    /** Generate a new Ed25519 key pair for Cardano. */
    def generateKeyPair(): IO[(VerificationKey, SigningKey)] =
        IO {
            val random = new SecureRandom()
            val generator = new Ed25519KeyPairGenerator()
            generator.init(new Ed25519KeyGenerationParameters(random))

            val keyPair = generator.generateKeyPair()

            val privateKey = keyPair.getPrivate.asInstanceOf[Ed25519PrivateKeyParameters]
            val publicKey = keyPair.getPublic.asInstanceOf[Ed25519PublicKeyParameters]

            val privateKeyBytes: Array[Byte] = privateKey.getEncoded // 32 bytes (seed)
            val publicKeyBytes: Array[Byte] = publicKey.getEncoded // 32 bytes

            val vKey = VerificationKey.unsafeFromByteString(ByteString.fromArray(publicKeyBytes))
            val sKey = SigningKey.unsafeFromByteString(ByteString.fromArray(privateKeyBytes))

            (vKey, sKey)
        }

    // Shared peer-topology decoders, used by both `Membership` (the roster) and `BootstrapConfig`
    // (the full config). Nested in the enclosing object so both companions see them.
    private given Decoder[Uri] =
        Decoder.decodeString.emap(Uri.fromString(_).left.map(_.message))
    private given Decoder[HeadPeerData] = deriveDecoder[HeadPeerData]

    /** The peer topology (`roster.json`): each head peer's verification key + WebSocket address and
      * each coil peer's verification key + hub head peer. [[GenerateKeyPair]] writes this;
      * [[readBootstrapDir]] assembles it with the rest of the bootstrap directory into a
      * [[BootstrapConfig]]. Head peer 0 is the first entry in [[headPeers]].
      */
    final case class Membership(
        headPeers: List[HeadPeerData],
        coilPeers: List[CoilPeerData]
    )

    object Membership {
        given Decoder[Membership] = deriveDecoder[Membership]
    }

    /** The head's protocol parameters as carried in the bootstrap config — the spec's `headParams`
      * cluster: transaction timing, the fallback contingency, the dispute-resolution and settlement
      * configs, and the coil quorum. [[BuildHeadConfig]] completes it into the full
      * [[HeadParameters]] by adding the code-pinned `l2Ledger` / `identityIsomorphism` /
      * `l2ParamsHash`.
      */
    final case class BootstrapHeadParams(
        txTiming: TxTiming,
        fallbackContingency: FallbackContingency,
        disputeResolutionConfig: DisputeResolutionConfig,
        settlementConfig: SettlementConfig,
        coilQuorum: Int
    )

    object BootstrapHeadParams {
        given Encoder[BootstrapHeadParams] = deriveEncoder[BootstrapHeadParams]
        given (using CardanoNetwork.Section): Decoder[BootstrapHeadParams] =
            deriveDecoder[BootstrapHeadParams]
    }

    /** Assembly-time defaults (`defaults.json`): everything the head needs that is neither peer
      * topology (the roster), script references, nor the opening L2 state — the L1 network, the
      * head protocol parameters, the per-peer equity contributions, and (optionally) the block-zero
      * timing anchors. [[InitBootstrapFiles]] writes these with demo defaults for the operators to
      * adjust before [[BuildHeadConfig]]. Block-zero timing is optional: omit it and
      * [[BuildHeadConfig]] anchors the initial block to wall-clock at build time.
      */
    final case class BootstrapDefaults(
        cardanoNetwork: CardanoNetwork,
        headParams: BootstrapHeadParams,
        initialEquityContributions: Map[HeadPeerNumber, Coin],
        blockZeroStartTime: Option[BlockCreationStartTime],
        blockZeroEndTime: Option[BlockCreationEndTime]
    )

    object BootstrapDefaults {
        given (using CardanoNetwork.Section): Decoder[BootstrapDefaults] =
            deriveDecoder[BootstrapDefaults]
        given (using CardanoNetwork.Section): Encoder[BootstrapDefaults] =
            deriveEncoder[BootstrapDefaults]
    }

    /** One opening L2 output (`l2-cardano-eutxo.json` entry): a bech32 address and a CIP-0116
      * value. This is an EUTXO utxo minus its input reference — [[BuildHeadConfig]] assigns the
      * reference (the seed utxo's tx id + a per-entry index) once the seed is resolved. (A datum
      * field can be added here later; the opening distribution does not need one.)
      */
    final case class L2Output(address: Address, value: Value) {
        def toTransactionOutput: TransactionOutput =
            TransactionOutput.Babbage(address, value, None, None)
    }

    object L2Output {
        given Decoder[L2Output] = Decoder.instance { c =>
            for {
                addressStr <- c.downField("address").as[String]
                address <- Try(Address.fromBech32(addressStr)).toEither.left.map(e =>
                    DecodingFailure(s"invalid bech32 address: ${e.getMessage}", c.history)
                )
                value <- c.downField("value").as[Value]
            } yield L2Output(address, value)
        }

        given Encoder[L2Output] = Encoder.instance { o =>
            val addressBech32 = o.address match {
                case s: ShelleyAddress => s.toBech32.getOrElse(s.toHex)
                case a                 => a.toString
            }
            Json.obj("address" -> Json.fromString(addressBech32), "value" -> o.value.asJson)
        }
    }

    /** The bootstrap config: the assembled content of the bootstrap directory's four
      * operator-facing files ([[readBootstrapDir]]), which [[BuildHeadConfig]] turns into the
      * shared [[HeadConfig]]. It carries the L1 network, the head protocol parameters, the peer
      * topology, the pre-deployed script references, the opening L2 state, the per-peer equity
      * contributions, and (optionally) the block-zero timing.
      *
      * The seed utxo and the funding utxos are still not carried here: the demo uses the simplified
      * model where head peer 0 funds the head, so [[BuildHeadConfig]] resolves the seed + funding
      * from head peer 0's own L1 address at build time.
      */
    final case class BootstrapConfig(
        cardanoNetwork: CardanoNetwork,
        headParams: BootstrapHeadParams,
        headPeers: List[HeadPeerData],
        coilPeers: List[CoilPeerData],
        scriptReferenceUtxos: ScriptReferenceUtxos.Unresolved,
        initialL2State: List[L2Output],
        initialEquityContributions: Map[HeadPeerNumber, Coin],
        blockZeroStartTime: Option[BlockCreationStartTime],
        blockZeroEndTime: Option[BlockCreationEndTime]
    )

    /** The bootstrap directory's file names — the operator-facing inputs [[readBootstrapDir]]
      * assembles into a [[BootstrapConfig]].
      */
    object BootstrapDir {
        val roster = "roster.json"
        val defaults = "defaults.json"
        val l2CardanoEutxo = "l2-cardano-eutxo.json"
        val scriptRefs = "script-refs.json"

        /** The committed per-network script-refs defaults (`preview.json`, `preprod.json`, …) that
          * [[readBootstrapDir]] falls back to when the bootstrap directory carries no
          * [[scriptRefs]] file.
          */
        val defaultScriptRefsDir: Path = Path.of("config", "script-refs")
    }

    /** Read the bootstrap directory's four files ([[BootstrapDir]]) and assemble them into the
      * [[BootstrapConfig]]. The defaults decode against their own `cardanoNetwork` (the timing
      * codecs re-quantize against its slot config), so the network is read first and the rest
      * decoded with it in scope. A missing `script-refs.json` falls back to the committed default
      * for the network ([[BootstrapDir.defaultScriptRefsDir]]).
      */
    def readBootstrapDir(dir: Path): IO[BootstrapConfig] = {
        def readJson(path: Path): IO[Json] =
            IO.blocking(Files.readString(path))
                .flatMap(s => IO.fromEither(parser.parse(s)))
        for {
            rosterJson <- readJson(dir.resolve(BootstrapDir.roster))
            roster <- IO.fromEither(rosterJson.as[Membership])
            defaultsJson <- readJson(dir.resolve(BootstrapDir.defaults))
            network <- IO.fromEither(defaultsJson.hcursor.get[CardanoNetwork]("cardanoNetwork"))
            defaults <- {
                given CardanoNetwork.Section = network
                IO.fromEither(defaultsJson.as[BootstrapDefaults])
            }
            l2StateJson <- readJson(dir.resolve(BootstrapDir.l2CardanoEutxo))
            l2State <- IO.fromEither(l2StateJson.as[List[L2Output]])
            scriptRefsPath <- resolveScriptRefsPath(dir, network)
            scriptRefsJson <- readJson(scriptRefsPath)
            scriptRefs <- IO.fromEither(scriptRefsJson.as[ScriptReferenceUtxos.Unresolved])
        } yield BootstrapConfig(
          cardanoNetwork = defaults.cardanoNetwork,
          headParams = defaults.headParams,
          headPeers = roster.headPeers,
          coilPeers = roster.coilPeers,
          scriptReferenceUtxos = scriptRefs,
          initialL2State = l2State,
          initialEquityContributions = defaults.initialEquityContributions,
          blockZeroStartTime = defaults.blockZeroStartTime,
          blockZeroEndTime = defaults.blockZeroEndTime
        )
    }

    /** Pick the script-refs source: the bootstrap directory's own `script-refs.json` when present,
      * else the committed per-network default. Fails naming both candidates when neither exists —
      * the network has no default yet, so run `just deploy-reference-scripts` first.
      */
    private def resolveScriptRefsPath(dir: Path, network: CardanoNetwork): IO[Path] = {
        val own = dir.resolve(BootstrapDir.scriptRefs)
        val default = BootstrapDir.defaultScriptRefsDir
            .resolve(s"${network.toString.toLowerCase}.json")
        IO.blocking {
            if Files.exists(own) then own
            else if Files.exists(default) then default
            else
                throw RuntimeException(
                  s"no script refs: neither $own nor the committed default $default exists — " +
                      "run `just deploy-reference-scripts` first"
                )
        }
    }

    /** Build the shared [[HeadConfig]] every node loads, for an N-head + M-coil head.
      *
      * The head protocol parameters, the per-peer equity contributions, and (optionally) the
      * block-zero timing all come from the bootstrap config. Head peer 0 funds the head: it
      * supplies the seed and every funding utxo from its own L1 enterprise address, covering the
      * total equity plus the whole head's fallback contingency (the collective share once, plus one
      * individual share per head peer). Block-zero timing falls back to wall-clock when the config
      * omits it.
      */
    def mkSharedHeadConfig(cardanoNetwork: CardanoNetwork, backend: CardanoBackend[IO])(
        bootstrapConfig: BootstrapConfig,
        scriptReferenceUtxos: ScriptReferenceUtxos
    ): IO[HeadConfig] = for {
        blockCreationStartTime <- bootstrapConfig.blockZeroStartTime.fold(
          realTimeQuantizedInstant(cardanoNetwork.slotConfig).map(BlockCreationStartTime(_))
        )(IO.pure)

        bhp = bootstrapConfig.headParams
        headParams = HeadParameters(
          txTiming = bhp.txTiming,
          fallbackContingency = bhp.fallbackContingency,
          disputeResolutionConfig = bhp.disputeResolutionConfig,
          settlementConfig = bhp.settlementConfig,
          coilQuorum = bhp.coilQuorum,
          // Placeholder: the L2 params hash is not consumed yet. Hash32 requires 32 bytes, so use
          // a zero hash rather than empty bytes (which fail the length check).
          l2ParamsHash = Hash32.fromByteString(ByteString.fromArray(new Array[Byte](32))),
          // The demo build targets the built-in EUTXO ledger. TODO: surface via a --l2-ledger flag.
          l2Ledger = L2LedgerKind.CardanoEutxo,
          // Enforce the headId pin (format isomorphism only). TODO: surface via a flag.
          identityIsomorphism = false,
        )

        // The opening L2 state's total value is what the treasury must back on L1 (the initial L2
        // value). The evacuation map itself is built later, once the seed utxo — the source of the
        // synthetic key ids — is resolved.
        initialL2Value = Value.combine(bootstrapConfig.initialL2State.map(_.value))

        // Equity comes from the config (per head peer). Its total is what the treasury backs beyond
        // the L2 value; the per-peer split is recorded but the init tx consumes only the sum.
        initialEquityContributions <- IO.fromOption(
          NonEmptyMap.fromMap(SortedMap.from(bootstrapConfig.initialEquityContributions))
        )(RuntimeException("initialEquityContributions must be non-empty"))
        totalEquity = initialEquityContributions.toSortedMap.values.foldLeft(Coin.zero)(_ + _)

        headPeers <- IO.fromOption(
          HeadPeers(
            NonEmptyMap.fromMapUnsafe(
              SortedMap.from(
                bootstrapConfig.headPeers.zipWithIndex.map((data, i) => HeadPeerNumber(i) -> data)
              )
            )
          )
        )(RuntimeException("head peers must be a non-empty list (head peer 0 first)"))

        coilPeers = CoilPeers.indexed(bootstrapConfig.coilPeers)
        nHeadPeers = bootstrapConfig.headPeers.size

        // Head peer 0 is the sole funder; its L1 enterprise address supplies the seed and funding.
        funderVKey = bootstrapConfig.headPeers.head.verificationKey
        peerAddress = funderVKey.shelleyAddress()(using cardanoNetwork)
        _ <- logger.info(s"Funder (head peer 0) address: ${peerAddress.toBech32.get}")

        // Fetch UTXOs from backend
        utxosResult <- backend.utxosAt(peerAddress)
        utxosMap <- IO.fromEither(
          utxosResult.left.map(err =>
              new RuntimeException(s"Failed to fetch UTXOs from backend: $err")
          )
        )

        // The whole head's fallback contingency, all funded by head peer 0: the collective share
        // once plus one individual share per head peer.
        fallbackContingency = headParams.fallbackContingency
        totalContingency = Coin(
          fallbackContingency.collectiveContingency.total.value +
              nHeadPeers.toLong * fallbackContingency.individualContingency.total.value
        )

        // Calculate required value: total equity + total head contingency + initial L2 value +
        // max non-Plutus tx fee for initialization
        maxNonPlutusTxFee = cardanoNetwork.maxNonPlutusTxFee
        requiredValue = Value(totalEquity + totalContingency + maxNonPlutusTxFee) + initialL2Value
        _ <- logger.info(
          s"Required value: ${totalEquity.value} lovelace (equity) + " +
              s"${totalContingency.value} lovelace (total head contingency) + " +
              s"${initialL2Value.coin.value} lovelace (initial L2 value) + " +
              s"${maxNonPlutusTxFee.value} lovelace (fee) = ${requiredValue.coin.value} lovelace"
        )

        // Select UTXOs that cover the required value
        utxosSelected <- {
            if utxosMap.isEmpty then {
                logger.error(s"No UTXOs found at address ${peerAddress.toBech32.get}") *>
                    IO.raiseError(
                      new RuntimeException(
                        s"No UTXOs available at address ${peerAddress.toBech32.get}. " +
                            s"Please fund this address with at least ${requiredValue.coin.value} lovelace."
                      )
                    )
            } else {
                // Accumulate UTXOs until we have enough value (only ADA, no multi-assets)
                val utxosList = utxosMap.toList.map { case (txIn, txOut) => Utxo(txIn, txOut) }
                val (selected, accumulated) =
                    utxosList.foldLeft((List.empty[Utxo], Value.lovelace(0))) {
                        case (acc @ (selectedUtxos, accValue), utxo) =>
                            if accValue.coin.value >= requiredValue.coin.value then acc
                            else (selectedUtxos :+ utxo, accValue + utxo.output.value)
                    }

                if accumulated.coin.value < requiredValue.coin.value then {
                    val errorMsg =
                        s"Insufficient funds at address ${peerAddress.toBech32.get}. " +
                            s"Available: ${accumulated.coin.value} lovelace, " +
                            s"Required: ${requiredValue.coin.value} lovelace (${totalEquity.value} equity + ${maxNonPlutusTxFee.value} fee). " +
                            s"Please fund address ${peerAddress.toBech32.get} with at least ${requiredValue.coin.value - accumulated.coin.value} more lovelace."
                    logger.error(errorMsg) *>
                        IO.raiseError(new RuntimeException(errorMsg))
                } else {
                    NonEmptyList.fromList(selected) match {
                        case Some(nel) =>
                            logger.info(
                              s"Selected ${nel.size} UTXO(s) with total value: ${accumulated.coin.value} lovelace"
                            ) *>
                                IO.pure(nel)
                        case None =>
                            // This should never happen due to the isEmpty check above
                            IO.raiseError(
                              new RuntimeException("Internal error: selected UTXOs list is empty")
                            )
                    }
                }
            }
        }

        seedUtxo = utxosSelected.head
        valueSelected = Value.combine(utxosSelected.map(_.output.value).toList)

        // The build-time funding recipe (seed + funding utxos + change). The head id is derived
        // from the seed here, in the bootstrap module, and presented explicitly in the config.
        funding = InitializationFunding(
          seedUtxo = seedUtxo,
          additionalFundingUtxos = utxosSelected.tail.map(_.toTuple).toMap,
          changeOutputs = List(
            TransactionOutput.Babbage(
              address = peerAddress,
              value = valueSelected - Value(totalEquity) -
                  Value(totalContingency) - initialL2Value,
              datumOption = None,
              scriptRef = None
            )
          )
        )

        // Assign each opening L2 output a synthetic utxo reference and project the outputs into the
        // initial evacuation map (which also validates min-ada). The reference's tx id is the
        // genesis hash of the *whole* seed input (tx id + index) — the same synthetic-id convention
        // the EUTXO ledger uses for deposit-created utxos (`L2Genesis.mkGenesisId`). Reusing the
        // seed's raw tx id would be unsafe: `(seedTxId, i)` could collide with a real on-chain output
        // of the seed's own transaction, or with the seed utxo itself.
        genesisTxId = L2Genesis.mkGenesisId(seedUtxo.input)
        initialUtxos: Utxos = bootstrapConfig.initialL2State.zipWithIndex.map { case (out, i) =>
            TransactionInput(genesisTxId, i) -> out.toTransactionOutput
        }.toMap
        evacMap <- IO.fromEither(
          initialUtxos.toEvacuationMap(cardanoNetwork).left.map(e => RuntimeException(e.toString))
        )

        initializationParameters = InitializationParameters(
          initialEvacuationMap = evacMap,
          initialEquityContributions = initialEquityContributions,
          headId = funding.headId
        )

        bootstrap <- IO.fromEither(
          HeadConfig
              .Bootstrap(
                cardanoNetwork = cardanoNetwork,
                headParams = headParams,
                headPeers = headPeers,
                coilPeers = coilPeers,
                initializationParams = initializationParameters,
                scriptReferenceUtxos = scriptReferenceUtxos
              )
              .toEither
              .left
              .map(errors =>
                  RuntimeException(
                    "could not construct HeadConfig.bootstrap during bootstrap. Errors:" +
                        s"${errors.foldLeft("\n\t")(_ ++ _)}"
                  )
              )
        )

        blockCreationEndTime <- bootstrapConfig.blockZeroEndTime.fold(
          IO.realTimeInstant.map(instant =>
              BlockCreationEndTime(instant.quantize(cardanoNetwork.slotConfig))
          )
        )(IO.pure)

        initTxSeq <- InitializationTxSeq
            .Build(bootstrap, funding)(blockCreationEndTime)
            .result
            .fold(
              e => logger.error(e.toString) >> IO.raiseError(e),
              IO.pure
            )

        fallbackTxStartTime = initTxSeq.fallbackTx.fallbackTxStartTime
        forcedMajorBlockWakeupTime = headParams.txTiming.forcedMajorBlockWakeupTime(
          fallbackTxStartTime
        )

        initialBlock = InitialBlock(
          Block.Unsigned.Initial(
            blockBrief = BlockBrief.Initial(
              BlockHeader.Initial(
                startTime = blockCreationStartTime,
                endTime = blockCreationEndTime,
                fallbackTxStartTime = fallbackTxStartTime,
                forcedMajorBlockWakeupTime = forcedMajorBlockWakeupTime,
                mDepositDecisionWakeupTime = None,
              )
            ),
            // Unsigned init+fallback — slow consensus's stack-0 hard-ack flow signs them at boot.
            effects = BlockEffects.Unsigned.Initial(
              initializationTx = initTxSeq.initializationTx,
              fallbackTx = initTxSeq.fallbackTx
            )
          )
        )

        headConfig <- HeadConfig(
          headConfigBootstrap = bootstrap,
          initialBlock = initialBlock
        ) match {
            case Validated.Valid(hc) => IO.pure(hc)
            case Validated.Invalid(_) =>
                IO.raiseError(
                  RuntimeException("failure building head config; get logs for details")
                )
        }
    } yield headConfig

end Bootstrap

/** Main entry point for generating a new Ed25519 key pair.
  *
  * With no options, prints the verification key and signing key as hex to stdout (the historical
  * behavior). Options add two file outputs, both meant to be run once per peer when cooking a
  * head's configs:
  *
  *   - `--roster roster.json --role head --ws-address ws://…` (or `--role coil --hub N`) creates or
  *     appends to the [[Bootstrap.Membership]] roster — the peer-topology part of the bootstrap
  *     config that [[BuildHeadConfig]] consumes (together with the defaults, opening L2 state, and
  *     script references in the same bootstrap directory). Head peers must be registered before the
  *     coil peers that name them as hubs.
  *   - `--template private-template.json --out private.json` writes the peer's private node config:
  *     the template with `ownPeerPrivate` replaced by the generated wallet (as `ownHeadWallet` /
  *     `ownCoilWallet` per `--role`) and a second fresh key pair spliced in as the evacuation
  *     `ruleBasedWallet`. The splice works on the JSON tree because the stock config encoders
  *     deliberately withhold signing keys ([[PeerWallet.dummyPeerWalletEncoder]]). Head peer 0's L1
  *     address must be funded before [[BuildHeadConfig]] — print it with [[PrintHeadZeroAddress]].
  *
  * Usage:
  * {{{
  *   sbt "runMain hydrozoa.bootstrap.GenerateKeyPair \
  *     --roster nodes/roster.json --role head --ws-address ws://head-0:4001 \
  *     --template config/template/peer-private.template.json --out nodes/head-0/private.json"
  * }}}
  */
object GenerateKeyPair
    extends CommandIOApp(
      name = "keygen",
      header = "Generate an Ed25519 key pair; optionally register it in a roster " +
          "and write a private node config"
    ):

    /** The peer role a generated key pair is registered under. */
    enum Role:
        case Head, Coil

    private val rosterOpt: Opts[Option[Path]] =
        Opts.option[String](
          "roster",
          "Membership roster JSON to create or append to (the BuildHeadConfig input)"
        ).map(Path.of(_))
            .orNone

    private val roleOpt: Opts[Option[Role]] =
        Opts.option[String]("role", "Peer role: head | coil")
            .mapValidated {
                case "head" => Validated.validNel(Role.Head)
                case "coil" => Validated.validNel(Role.Coil)
                case other => Validated.invalidNel(s"--role must be 'head' or 'coil', got '$other'")
            }
            .orNone

    private val wsAddressOpt: Opts[Option[Uri]] =
        Opts.option[String](
          "ws-address",
          "This head peer's inter-peer WebSocket address, e.g. ws://head-0:4001"
        ).mapValidated(s =>
            Validated.fromEither(Uri.fromString(s).left.map(_.message)).toValidatedNel
        ).orNone

    private val hubOpt: Opts[Option[Int]] =
        Opts.option[Int]("hub", "The head peer number that hubs this coil peer").orNone

    private val templateOpt: Opts[Option[Path]] =
        Opts.option[String](
          "template",
          "Private-config template JSON to fill in with the generated keys"
        ).map(Path.of(_))
            .orNone

    private val outOpt: Opts[Option[Path]] =
        Opts.option[String]("out", "Where to write the filled-in private config", short = "o")
            .map(Path.of(_))
            .orNone

    override def main: Opts[IO[ExitCode]] =
        (rosterOpt, roleOpt, wsAddressOpt, hubOpt, templateOpt, outOpt)
            .mapN(generateAndWrite)

    /** Generate the key pair, print it, and perform whichever file outputs the options request. */
    private def generateAndWrite(
        roster: Option[Path],
        role: Option[Role],
        wsAddress: Option[Uri],
        hub: Option[Int],
        template: Option[Path],
        out: Option[Path]
    ): IO[ExitCode] = for {
        _ <- validateOptionCombos(roster, role, wsAddress, hub, template, out)
        keyPair <- Bootstrap.generateKeyPair()
        (vKey, sKey) = keyPair
        vKeyHex = mkHex(vKey.bytes.toArray)
        sKeyHex = mkHex(sKey.bytes.toArray)
        address = vKey.shelleyAddress()(using CardanoNetwork.Preview).toBech32.get
        _ <- IO.println("Generated new Ed25519 key pair:")
        _ <- IO.println(s"Verification key (32 bytes): $vKeyHex")
        _ <- IO.println(s"Signing key (32 bytes): $sKeyHex")
        _ <- IO.println(s"Testnet address: $address")
        _ <- roster.traverse_(path =>
            registerInRoster(path, role.get, vKeyHex, wsAddress, hub) *>
                IO.println(s"Registered ${role.get.toString.toLowerCase} peer in roster: $path")
        )
        _ <- (template, out).tupled.traverse_ { (templatePath, outPath) =>
            writePrivateConfig(templatePath, outPath, role.get, vKeyHex, sKeyHex) *>
                IO.println(s"Wrote private config: $outPath")
        }
    } yield ExitCode.Success

    /** Reject option combinations the file outputs can't act on. */
    private def validateOptionCombos(
        roster: Option[Path],
        role: Option[Role],
        wsAddress: Option[Uri],
        hub: Option[Int],
        template: Option[Path],
        out: Option[Path]
    ): IO[Unit] = {
        def fail(msg: String): IO[Unit] = IO.raiseError(new IllegalArgumentException(msg))
        for {
            _ <-
                if (roster.isDefined || out.isDefined) && role.isEmpty then
                    fail("--role is required with --roster or --out")
                else IO.unit
            _ <-
                if roster.isDefined && role.contains(Role.Head) && wsAddress.isEmpty then
                    fail("--ws-address is required for --role head with --roster")
                else IO.unit
            _ <-
                if roster.isDefined && role.contains(Role.Coil) && hub.isEmpty then
                    fail("--hub is required for --role coil with --roster")
                else IO.unit
            _ <-
                if template.isDefined != out.isDefined then
                    fail("--template and --out must be given together")
                else IO.unit
        } yield ()
    }

    /** Append this peer to the roster (creating the file if absent), keyed by role, and validate
      * the result still decodes as a [[Bootstrap.Membership]].
      */
    private def registerInRoster(
        path: Path,
        role: Role,
        vKeyHex: String,
        wsAddress: Option[Uri],
        hub: Option[Int]
    ): IO[Unit] = for {
        current <- IO.blocking(Files.exists(path)).flatMap {
            case true =>
                IO.blocking(Files.readString(path))
                    .flatMap(s => IO.fromEither(parser.parse(s)))
            case false => IO.pure(emptyRoster)
        }
        updated <- IO.fromEither(
          appendPeer(current, role, vKeyHex, wsAddress, hub).left
              .map(new IllegalArgumentException(_))
        )
        _ <- IO.fromEither(
          updated
              .as[Bootstrap.Membership]
              .left
              .map(err =>
                  new IllegalStateException(s"updated roster does not decode as a Membership: $err")
              )
        )
        _ <- IO.blocking {
            Option(path.getParent).foreach(Files.createDirectories(_))
            Files.writeString(path, updated.spaces2)
        }
    } yield ()

    private[bootstrap] val emptyRoster: Json = Json.obj(
      "headPeers" -> Json.arr(),
      "coilPeers" -> Json.arr()
    )

    /** Pure roster edit: append the peer entry for its role. */
    private[bootstrap] def appendPeer(
        roster: Json,
        role: Role,
        vKeyHex: String,
        wsAddress: Option[Uri],
        hub: Option[Int]
    ): Either[String, Json] = for {
        obj <- roster.asObject.toRight("roster is not a JSON object")
        headPeers = obj("headPeers").flatMap(_.asArray).getOrElse(Vector.empty)
        coilPeers = obj("coilPeers").flatMap(_.asArray).getOrElse(Vector.empty)
        withPeer <- role match {
            case Role.Head =>
                wsAddress.toRight("--ws-address is required for a head peer").map { ws =>
                    val entry = Json.obj(
                      "verificationKey" -> Json.fromString(vKeyHex),
                      "webSocketAddress" -> Json.fromString(ws.renderString)
                    )
                    obj.add("headPeers", Json.fromValues(headPeers :+ entry))
                }
            case Role.Coil =>
                for {
                    hubNum <- hub.toRight("--hub is required for a coil peer")
                    _ <- Either.cond(
                      hubNum >= 0 && hubNum < headPeers.size,
                      (),
                      s"--hub $hubNum is not a registered head peer " +
                          s"(roster has ${headPeers.size}; register head peers first)"
                    )
                } yield {
                    val entry = Json.obj(
                      "verificationKey" -> Json.fromString(vKeyHex),
                      "hubHeadPeerNumber" -> Json.fromInt(hubNum)
                    )
                    obj.add("coilPeers", Json.fromValues(coilPeers :+ entry))
                }
        }
    } yield Json.fromJsonObject(withPeer)

    /** Fill the private-config template: set `ownPeerPrivate` to the generated wallet under the
      * role's field name, and replace the evacuation `ruleBasedWallet` with a fresh key pair.
      */
    private def writePrivateConfig(
        templatePath: Path,
        outPath: Path,
        role: Role,
        vKeyHex: String,
        sKeyHex: String
    ): IO[Unit] = for {
        templateStr <- IO.blocking(Files.readString(templatePath))
        template <- IO.fromEither(parser.parse(templateStr))
        ruleBasedKeyPair <- Bootstrap.generateKeyPair()
        (rbVKey, rbSKey) = ruleBasedKeyPair
        filled <- IO.fromEither(
          fillPrivateConfig(
            template,
            role,
            vKeyHex,
            sKeyHex,
            mkHex(rbVKey.bytes.toArray),
            mkHex(rbSKey.bytes.toArray)
          ).left.map(new IllegalArgumentException(_))
        )
        _ <- IO.blocking {
            Option(outPath.getParent).foreach(Files.createDirectories(_))
            Files.writeString(outPath, filled.spaces2)
        }
    } yield ()

    /** Pure template edit shared with tests. */
    private[bootstrap] def fillPrivateConfig(
        template: Json,
        role: Role,
        vKeyHex: String,
        sKeyHex: String,
        ruleBasedVKeyHex: String,
        ruleBasedSKeyHex: String
    ): Either[String, Json] = {
        def mkWallet(v: String, s: String): Json = Json.obj(
          "verificationKey" -> Json.fromString(v),
          "signingKey" -> Json.fromString(s)
        )
        val walletField = role match {
            case Role.Head => "ownHeadWallet"
            case Role.Coil => "ownCoilWallet"
        }
        for {
            obj <- template.asObject.toRight("template is not a JSON object")
            evac <- obj("nodeOperationEvacuationConfig")
                .flatMap(_.asObject)
                .toRight("template missing nodeOperationEvacuationConfig object")
        } yield Json.fromJsonObject(
          obj
              .add("ownPeerPrivate", Json.obj(walletField -> mkWallet(vKeyHex, sKeyHex)))
              .add(
                "nodeOperationEvacuationConfig",
                Json.fromJsonObject(
                  evac.add("ruleBasedWallet", mkWallet(ruleBasedVKeyHex, ruleBasedSKeyHex))
                )
              )
        )
    }

    private def mkHex(bytes: Array[Byte]): String = bytes.map("%02x".format(_)).mkString

end GenerateKeyPair

/** Migrate all funds from the configured peer's own L1 address to a destination address.
  *
  * Usage:
  * {{{
  *   sbt "runMain hydrozoa.bootstrap.Migrate <head-config.json> <peer-private.json> <bech32-dest>"
  * }}}
  *
  * Reads wallet + Blockfrost credentials from the same files Main does; signs with the peer wallet
  * carried by the private config.
  *
  * Sweeps only the UTxOs sitting at the peer wallet's own enterprise address, with that wallet's
  * single signature. It never spends from the head's multisig address, so it is NOT a recovery tool
  * for an initialized multi-peer head — funds locked in the head come back only through
  * finalization (all head signatures + coil quorum) or the rule-based evacuation regime. The
  * "recover the head's funds" reading held only for the single-peer demo, where finalization
  * returns everything to peer 0's own address and this tool then sweeps it.
  */
object Migrate
    extends CommandIOApp(
      name = "migrate",
      header = "Send all UTXOs at the peer's wallet address to a destination address"
    ):

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("hydrozoa.bootstrap.Migrate"))

    private val headArg: Opts[Path] =
        Opts.argument[String]("head-config.json").map(Path.of(_))
    private val privateArg: Opts[Path] =
        Opts.argument[String]("peer-private.json").map(Path.of(_))
    private val destArg: Opts[String] = Opts.argument[String]("bech32-destination")

    override def main: Opts[IO[ExitCode]] =
        (headArg, privateArg, destArg).mapN(migrateAllFunds)

    private def migrateAllFunds(
        headConfigPath: Path,
        privateConfigPath: Path,
        destinationBech32: String
    ): IO[ExitCode] =
        for {
            _ <- log.info(s"Starting migration to $destinationBech32")

            loaded <- NodeConfig.load(headConfigPath, privateConfigPath)
            (nodeConfig, backend) = loaded

            destination <- IO
                .delay(Address.fromBech32(destinationBech32))
                .flatMap {
                    case addr: scalus.cardano.address.ShelleyAddress =>
                        log.info(s"Destination address: ${addr.toBech32.get}") *>
                            IO.pure(addr)
                    case other =>
                        IO.raiseError(
                          new IllegalArgumentException(
                            s"Destination must be a Shelley address, got: ${other.getClass.getSimpleName}"
                          )
                        )
                }
                .handleErrorWith(err =>
                    IO.raiseError(new IllegalArgumentException(s"Invalid bech32 address: $err"))
                )

            wallet = nodeConfig.ownWallet
            cardanoNetwork: StandardCardanoNetwork =
                nodeConfig.cardanoNetwork match {
                    case n: StandardCardanoNetwork => n
                    case _ =>
                        throw new IllegalStateException(
                          "Migrate requires a standard Cardano network in the head config"
                        )
                }
            peerAddress = wallet.exportVerificationKey.shelleyAddress()(using cardanoNetwork)
            _ <- log.info(s"Peer address: ${peerAddress.toBech32.get}")

            _ <- log.info("Fetching UTXOs from peer address...")
            utxosResult <- backend.utxosAt(peerAddress)
            utxosMap <- IO.fromEither(
              utxosResult.left.map(err => new RuntimeException(s"Failed to fetch UTXOs: $err"))
            )

            _ <-
                if utxosMap.isEmpty then
                    log.warn("No UTXOs found at peer address. Nothing to migrate.")
                        .as(
                          ExitCode.Success
                        )
                else
                    for {
                        _ <- log.info(s"Found ${utxosMap.size} UTXO(s) to migrate")

                        totalValue = Value.combine(utxosMap.map((_, output) => output.value))
                        _ <- log.info(
                          s"Total value to migrate: ${totalValue.coin.value} lovelace + " +
                              s"${totalValue.assets.assets.size} asset(s)"
                        )

                        _ <- log.info("Building transaction...")
                        unbalanced = TransactionBuilder
                            .build(
                              cardanoNetwork.network,
                              utxosMap.map { case (utxoId, output) =>
                                  Spend(Utxo(utxoId, output))
                              }.toList :+
                                  Send(
                                    TransactionOutput.Babbage(
                                      address = destination,
                                      value = totalValue,
                                      datumOption = None,
                                      scriptRef = None
                                    )
                                  )
                            )
                            .fold(
                              err =>
                                  throw new RuntimeException(
                                    s"Failed to build transaction: $err"
                                  ),
                              identity
                            )

                        _ <- log.info("Balancing transaction...")
                        protocolParams <- backend.fetchLatestParams.flatMap(
                          IO.fromEither(_)
                              .adaptError(err =>
                                  new RuntimeException(s"Failed to fetch protocol params: $err")
                              )
                        )

                        balanced = unbalanced
                            .balanceContext(
                              diffHandler = Change.changeOutputDiffHandler(
                                _,
                                _,
                                protocolParams = protocolParams,
                                changeOutputIdx = 0
                              ),
                              protocolParams = protocolParams,
                              evaluator = PlutusScriptEvaluator(
                                cardanoNetwork.cardanoInfo,
                                EvaluatorMode.EvaluateAndComputeCost
                              )
                            )
                            .fold(
                              err =>
                                  throw new RuntimeException(
                                    s"Failed to balance transaction: $err"
                                  ),
                              _.transaction
                            )

                        _ <- log.info("Signing transaction...")
                        signed = wallet.signTx(balanced)

                        _ <- log.info(s"Transaction hash: ${signed.id}")
                        _ <- log.info(
                          s"Transaction CBOR: ${HexUtil.encodeHexString(signed.toCbor)}"
                        )

                        _ <- log.info("Submitting transaction...")
                        submitResult <- backend.submitTx(RawTx(signed))
                        _ <- IO.fromEither(
                          submitResult.left.map(err =>
                              new RuntimeException(s"Failed to submit transaction: $err")
                          )
                        )

                        _ <- log.info("Transaction submitted successfully")
                        _ <- log.info(
                          s"Explorer: https://preview.cexplorer.io/tx/${signed.id.toHex}"
                        )
                    } yield ExitCode.Success

        } yield ExitCode.Success

end Migrate

/** Build the shared head config artifact every node loads.
  *
  * Usage:
  * {{{
  *   sbt "runMain hydrozoa.bootstrap.BuildHeadConfig <bootstrap-dir> \
  *     --blockfrost-key <key> [--out head-config.json]"
  * }}}
  *
  * Reads the bootstrap directory's four operator-facing files — `roster.json` (the peer topology
  * keygen writes), `defaults.json` (network, head params, equity, optional block-zero timing),
  * `l2-cardano-eutxo.json` (the opening L2 state), and `script-refs.json` (from
  * [[DeployReferenceScripts]]) — assembles them into the [[Bootstrap.BootstrapConfig]], funds the
  * head from head peer 0's L1 address (via the Blockfrost backend), and writes the resulting
  * [[HeadConfig]] as JSON to `--out` (default `head-config.json`). Every node then loads this same
  * artifact.
  *
  * The script reference utxos are the L1 inputs of the treasury and dispute reference-script UTxOs
  * that [[DeployReferenceScripts]] deployed. They are resolved and hash-checked against
  * [[HydrozoaBlueprint]] here, so a stale deployment fails the build rather than every node's
  * startup.
  */
object BuildHeadConfig
    extends CommandIOApp(
      name = "build-head-config",
      header = "Build the shared head-config.json artifact every node loads"
    ):

    private val logger = Logging.loggerIO("hydrozoa.bootstrap.BuildHeadConfig")

    private val bootstrapDirArg: Opts[Path] =
        Opts.argument[String]("bootstrap-dir").map(Path.of(_))
    private val blockfrostKeyOpt: Opts[String] =
        Opts.option[String](
          "blockfrost-key",
          "Blockfrost API key for the Cardano backend (falls back to $BLOCKFROST_API_KEY)",
          short = "k"
        ).orElse(
          Opts.env[String]("BLOCKFROST_API_KEY", "Blockfrost API key for the Cardano backend")
        )
    private val outOpt: Opts[Path] =
        Opts.option[String]("out", "Output path (default head-config.json)", short = "o")
            .map(Path.of(_))
            .withDefault(Path.of("head-config.json"))

    override def main: Opts[IO[ExitCode]] =
        (bootstrapDirArg, blockfrostKeyOpt, outOpt).mapN(buildHeadConfig)

    private def buildHeadConfig(
        bootstrapDir: Path,
        blockfrostKey: String,
        outPath: Path
    ): IO[ExitCode] =
        for {
            bootstrapConfig <- Bootstrap.readBootstrapDir(bootstrapDir)
            cardanoNetwork = bootstrapConfig.cardanoNetwork
            _ <- logger.info(
              s"Building shared head config: ${bootstrapConfig.headPeers.size} head peer(s), " +
                  s"${bootstrapConfig.coilPeers.size} coil peer(s), " +
                  s"coil quorum ${bootstrapConfig.headParams.coilQuorum}"
            )
            backendTracer = Slf4jTracer.sink.contramap(CardanoBackendEventFormat.humanFormat)
            // Blockfrost only serves the standard networks.
            backend <- cardanoNetwork match {
                case n: StandardCardanoNetwork =>
                    CardanoBackendBlockfrost(Left(n), blockfrostKey, tracer = backendTracer)
                case CardanoNetwork.Custom(_, _) =>
                    IO.raiseError(
                      RuntimeException("The Blockfrost backend does not support a Custom network")
                    )
            }
            // Script reference utxos are carried in the bootstrap config as bare inputs; resolve
            // their outputs from the backend.
            scriptReferenceUtxos <- {
                given CardanoNetwork.Section = cardanoNetwork
                bootstrapConfig.scriptReferenceUtxos
                    .resolve(backend)
                    .flatMap(r =>
                        IO.fromEither(
                          r.left.map(e =>
                              RuntimeException(
                                "Failed to resolve script reference UTxOs from the bootstrap " +
                                    s"config: $e (redeploy with DeployReferenceScripts?)"
                              )
                          )
                        )
                    )
            }
            headConfig <- Bootstrap.mkSharedHeadConfig(cardanoNetwork, backend)(
              bootstrapConfig,
              scriptReferenceUtxos
            )
            _ <- IO.blocking(Files.writeString(outPath, headConfig.asJson.spaces2))
            _ <- logger.info(s"Wrote shared head config to $outPath")
        } yield ExitCode.Success

end BuildHeadConfig

/** Write the bootstrap directory's non-per-peer inputs ([[Bootstrap.BootstrapDir]]):
  * `defaults.json` (the L1 network, the head protocol parameters, and the per-peer equity
  * contributions — with demo defaults) and an `l2-cardano-eutxo.json` template (a min-ada
  * placeholder per head peer's L1 address, which the operators edit into the opening distribution).
  * Block-zero timing is left out of the defaults, so [[BuildHeadConfig]] anchors the initial block
  * to wall-clock at build time; operators who want to pin it add `blockZeroStartTime` /
  * `blockZeroEndTime` (epoch millis).
  *
  * Usage:
  * {{{
  *   sbt "runMain hydrozoa.bootstrap.InitBootstrapFiles <roster.json> [--out-dir .]"
  * }}}
  */
object InitBootstrapFiles
    extends CommandIOApp(
      name = "init-bootstrap-files",
      header = "Write defaults.json and an l2-cardano-eutxo.json template from a roster"
    ):

    private val logger = Logging.loggerIO("hydrozoa.bootstrap.InitBootstrapFiles")

    private val rosterArg: Opts[Path] =
        Opts.argument[String]("roster.json").map(Path.of(_))
    private val outDirOpt: Opts[Path] =
        Opts.option[String]("out-dir", "Directory for the generated files (default .)", short = "o")
            .map(Path.of(_))
            .withDefault(Path.of("."))
    private val coilQuorumOpt: Opts[Option[Int]] =
        Opts.option[Int]("coil-quorum", "Coil quorum (default: a simple majority of coil peers)")
            .orNone

    override def main: Opts[IO[ExitCode]] =
        (rosterArg, outDirOpt, coilQuorumOpt).mapN(init)

    private def init(
        rosterPath: Path,
        outDir: Path,
        coilQuorumOverride: Option[Int]
    ): IO[ExitCode] =
        for {
            rosterStr <- IO.blocking(Files.readString(rosterPath))
            roster <- IO.fromEither(parser.decode[Bootstrap.Membership](rosterStr))
            network = CardanoNetwork.Preview
            // Default coil quorum: a simple majority of the coil peers.
            coilQuorum = coilQuorumOverride.getOrElse(roster.coilPeers.size / 2 + 1)
            // Demo head parameters: the same values BuildHeadConfig used to hard-code, now surfaced
            // for the operators to adjust.
            headParams = Bootstrap.BootstrapHeadParams(
              txTiming = TxTiming.demo(network.slotConfig),
              fallbackContingency = network.mkFallbackContingencyWithDefaults(
                tallyTxFee = Coin.ada(3),
                voteTxFee = Coin.ada(3)
              ),
              disputeResolutionConfig = DisputeResolutionConfig.default(network.slotConfig),
              settlementConfig = SettlementConfig(PositiveInt.unsafeApply(100)),
              coilQuorum = coilQuorum
            )
            // Demo equity: head peer 0 funds the whole head, the rest contribute zero and co-sign.
            equity = roster.headPeers.indices
                .map(i => HeadPeerNumber(i) -> (if i == 0 then Coin.ada(100) else Coin.zero))
                .toMap
            defaults = Bootstrap.BootstrapDefaults(network, headParams, equity, None, None)
            defaultsJson = {
                given CardanoNetwork.Section = network
                defaults.asJson.deepDropNullValues
            }
            // Opening-state template: a min-ada placeholder per head peer's L1 address, for the
            // operators to edit into the opening distribution (or empty, to fund entirely via
            // deposits).
            l2State = roster.headPeers.map(hpd =>
                Bootstrap.L2Output(
                  hpd.verificationKey.shelleyAddress()(using network),
                  Value.ada(5L)
                )
            )
            _ <- IO.blocking {
                Files.createDirectories(outDir)
                Files.writeString(
                  outDir.resolve(Bootstrap.BootstrapDir.defaults),
                  defaultsJson.spaces2
                )
                Files.writeString(
                  outDir.resolve(Bootstrap.BootstrapDir.l2CardanoEutxo),
                  l2State.asJson.spaces2
                )
            }
            _ <- logger.info(
              s"Wrote ${Bootstrap.BootstrapDir.defaults} (network=$network, " +
                  s"coilQuorum=$coilQuorum) and an ${Bootstrap.BootstrapDir.l2CardanoEutxo} " +
                  s"template (${l2State.size} head-peer entries) to $outDir"
            )
        } yield ExitCode.Success

end InitBootstrapFiles

/** Print head peer 0's L1 address — the funding target for the head (in the demo model head peer 0
  * funds everything). Deriving it from the roster on demand beats writing address files next to the
  * private configs: fewer stale copies lying around to send funds to.
  *
  * Reads only `roster.json` + `defaults.json`, so it works before the reference scripts are
  * deployed (funding comes first).
  *
  * Usage:
  * {{{
  *   sbt "runMain hydrozoa.bootstrap.PrintHeadZeroAddress [--bootstrap-dir config/demo/bootstrap]"
  * }}}
  */
object PrintHeadZeroAddress
    extends CommandIOApp(
      name = "head-zero-address",
      header = "Print head peer 0's L1 funding address"
    ):

    private val bootstrapDirOpt: Opts[Path] =
        Opts.option[String](
          "bootstrap-dir",
          "The bootstrap directory (roster.json + defaults.json)",
          short = "d"
        ).map(Path.of(_))
            .withDefault(Path.of("config/demo/bootstrap"))

    override def main: Opts[IO[ExitCode]] = bootstrapDirOpt.map(printHeadZeroAddress)

    private def printHeadZeroAddress(dir: Path): IO[ExitCode] =
        for {
            rosterStr <- IO.blocking(Files.readString(dir.resolve(Bootstrap.BootstrapDir.roster)))
            roster <- IO.fromEither(parser.decode[Bootstrap.Membership](rosterStr))
            headZero <- IO.fromOption(roster.headPeers.headOption)(
              RuntimeException("the roster has no head peers")
            )
            defaultsStr <- IO.blocking(
              Files.readString(dir.resolve(Bootstrap.BootstrapDir.defaults))
            )
            defaultsJson <- IO.fromEither(parser.parse(defaultsStr))
            network <- IO.fromEither(defaultsJson.hcursor.get[CardanoNetwork]("cardanoNetwork"))
            address <- IO.fromOption(
              headZero.verificationKey.shelleyAddress()(using network).toBech32.toOption
            )(RuntimeException("could not render head peer 0's address as bech32"))
            _ <- IO.println(address)
        } yield ExitCode.Success

end PrintHeadZeroAddress
