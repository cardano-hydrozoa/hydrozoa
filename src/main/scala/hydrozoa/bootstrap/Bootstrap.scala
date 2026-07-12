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
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockHeader}
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.l1.tx.RawTx
import hydrozoa.multisig.ledger.l1.txseq.InitializationTxSeq
import io.circe.generic.semiauto.deriveDecoder
import io.circe.syntax.*
import io.circe.{Decoder, Json, parser}
import java.nio.file.{Files, Path}
import java.security.SecureRandom
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.params.{Ed25519KeyGenerationParameters, Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}
import org.http4s.Uri
import scala.collection.immutable.SortedMap
import scalus.cardano.address.Address
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Coin, EvaluatorMode, Hash32, PlutusScriptEvaluator, TransactionOutput, Utxo, Value}
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

    /** The peer topology: each head peer's verification key + WebSocket address, each coil peer's
      * verification key + hub head peer, and the coil quorum. This is the portion of the bootstrap
      * config that [[GenerateKeyPair]] assembles into the roster; the operators complete it into a
      * full [[BootstrapConfig]]. Head peer 0 is the first entry in [[headPeers]].
      */
    final case class Membership(
        headPeers: List[HeadPeerData],
        coilPeers: List[CoilPeerData],
        coilQuorum: Int
    )

    object Membership {
        given Decoder[Membership] = deriveDecoder[Membership]
    }

    /** The bootstrap config the operators author and the build step turns into a shared
      * [[HeadConfig]] (the whitepaper's head-initialization "bootstrap config"). Extends the peer
      * topology ([[Membership]]) with the L1 network, the pre-deployed script reference utxos, and
      * the initial evacuation map.
      *
      * Equity, the seed utxo, and the funding utxos are not carried here yet: the demo uses the
      * simplified model where head peer 0 funds the head, resolving the seed + funding from its own
      * L1 address at build time.
      */
    final case class BootstrapConfig(
        cardanoNetwork: CardanoNetwork,
        headPeers: List[HeadPeerData],
        coilPeers: List[CoilPeerData],
        coilQuorum: Int,
        scriptReferenceUtxos: ScriptReferenceUtxos.Unresolved,
        initialEvacuationMap: EvacuationMap
    )

    object BootstrapConfig {
        given Decoder[BootstrapConfig] = Decoder.instance { c =>
            for {
                cardanoNetwork <- c.downField("cardanoNetwork").as[CardanoNetwork]
                headPeers <- c.downField("headPeers").as[List[HeadPeerData]]
                coilPeers <- c.downField("coilPeers").as[List[CoilPeerData]]
                coilQuorum <- c.downField("coilQuorum").as[Int]
                scriptReferenceUtxos <- c
                    .downField("scriptReferenceUtxos")
                    .as[ScriptReferenceUtxos.Unresolved]
                // The evacuation-map decoder needs the L1 network for its output addresses; this
                // config carries the network, so decode it first and supply it here.
                initialEvacuationMap <- {
                    given CardanoNetwork.Section = cardanoNetwork
                    c.downField("initialEvacuationMap").as[EvacuationMap]
                }
            } yield BootstrapConfig(
              cardanoNetwork,
              headPeers,
              coilPeers,
              coilQuorum,
              scriptReferenceUtxos,
              initialEvacuationMap
            )
        }
    }

    /** Build the shared [[HeadConfig]] every node loads, for an N-head + M-coil head.
      *
      * Only head peer 0 funds the head: it contributes [[minEquity]] as L2 equity and covers the
      * whole head's fallback contingency (the collective share once, plus one individual share per
      * head peer) from its own L1 enterprise address. Heads 1..N-1 contribute zero and just co-sign
      * the stack-0 initialization; coil peers fund nothing. The initial L2 ledger and L2 parameters
      * are empty, and Cardano preview is used.
      */
    def mkSharedHeadConfig(cardanoNetwork: CardanoNetwork, backend: CardanoBackend[IO])(
        bootstrapConfig: BootstrapConfig,
        minEquity: Coin,
        scriptReferenceUtxos: ScriptReferenceUtxos
    ): IO[HeadConfig] = for {
        startTimeInstant <- realTimeQuantizedInstant(cardanoNetwork.slotConfig)
        blockCreationStartTime = BlockCreationStartTime(startTimeInstant)

        headParams = HeadParameters(
          txTiming = TxTiming.demo(cardanoNetwork.slotConfig),
          fallbackContingency = cardanoNetwork.mkFallbackContingencyWithDefaults(
            tallyTxFee = Coin.ada(3),
            voteTxFee = Coin.ada(3)
          ),
          disputeResolutionConfig = DisputeResolutionConfig.default(cardanoNetwork.slotConfig),
          settlementConfig = SettlementConfig(PositiveInt.unsafeApply(100)),
          coilQuorum = bootstrapConfig.coilQuorum,
          // Placeholder: the L2 params hash is not consumed yet. Hash32 requires 32 bytes, so use
          // a zero hash rather than empty bytes (which fail the length check).
          l2ParamsHash = Hash32.fromByteString(ByteString.fromArray(new Array[Byte](32))),
          // The demo build targets the built-in EUTXO ledger. TODO: surface via a --l2-ledger flag.
          l2Ledger = L2LedgerKind.CardanoEutxo,
          // Enforce the headId pin (format isomorphism only). TODO: surface via a flag.
          identityIsomorphism = false,
        )

        // The initial L2 state comes from the bootstrap config as an evacuation map. Its total value
        // is what the treasury must back on L1 (the initial L2 value).
        evacMap = bootstrapConfig.initialEvacuationMap
        feeAccValue = Value.combine(evacMap.outputs.map(_.utxo.value.value))

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

        // Calculate required value: equity + total head contingency + evacuation fee account +
        // max non-Plutus tx fee for initialization
        maxNonPlutusTxFee = cardanoNetwork.maxNonPlutusTxFee
        requiredValue = Value(minEquity + totalContingency + maxNonPlutusTxFee) + feeAccValue
        _ <- logger.info(
          s"Required value: ${minEquity.value} lovelace (equity) + " +
              s"${totalContingency.value} lovelace (total head contingency) + " +
              s"${feeAccValue.coin.value} lovelace (evacuation fee account) + " +
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
                            s"Required: ${requiredValue.coin.value} lovelace (${minEquity.value} equity + ${maxNonPlutusTxFee.value} fee). " +
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

        // Only head peer 0 contributes equity; heads 1..N-1 contribute zero and just co-sign.
        initialEquityContributions = NonEmptyMap.fromMapUnsafe(
          SortedMap.from(
            bootstrapConfig.headPeers.zipWithIndex.map((_, i) =>
                HeadPeerNumber(i) -> (if i == 0 then minEquity else Coin.zero)
            )
          )
        )

        // The build-time funding recipe (seed + funding utxos + change). The head id is derived
        // from the seed here, in the bootstrap module, and presented explicitly in the config.
        funding = InitializationFunding(
          seedUtxo = seedUtxo,
          additionalFundingUtxos = utxosSelected.tail.map(_.toTuple).toMap,
          changeOutputs = List(
            TransactionOutput.Babbage(
              address = peerAddress,
              value = valueSelected - Value(minEquity) -
                  Value(totalContingency) - feeAccValue,
              datumOption = None,
              scriptRef = None
            )
          )
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

        endTimeInstant <- IO.realTimeInstant.map(instant =>
            instant.quantize(cardanoNetwork.slotConfig)
        )
        blockCreationEndTime = BlockCreationEndTime(endTimeInstant)

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
  *     config. The operators complete it into the `bootstrap.json` that [[BuildHeadConfig]]
  *     consumes by adding the network, script references, and initial evacuation map.
  *     `--coil-quorum N` sets the roster's coil quorum. Head peers must be registered before the
  *     coil peers that name them as hubs.
  *   - `--template private-template.json --out private.json` writes the peer's private node config:
  *     the template with `ownPeerPrivate` replaced by the generated wallet (as `ownHeadWallet` /
  *     `ownCoilWallet` per `--role`) and a second fresh key pair spliced in as the evacuation
  *     `ruleBasedWallet`. The splice works on the JSON tree because the stock config encoders
  *     deliberately withhold signing keys ([[PeerWallet.dummyPeerWalletEncoder]]). The peer's
  *     Preview L1 address is written next to `--out` as `address.txt` (head peer 0's must be funded
  *     before [[BuildHeadConfig]]).
  *
  * Usage:
  * {{{
  *   sbt "runMain hydrozoa.bootstrap.GenerateKeyPair \
  *     --roster nodes/roster.json --role head --ws-address ws://head-0:4001 \
  *     --template peer-private.template.json --out nodes/head-0/private.json"
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

    private val coilQuorumOpt: Opts[Option[Int]] =
        Opts.option[Int]("coil-quorum", "Set the roster's coilQuorum").orNone

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
        (rosterOpt, roleOpt, wsAddressOpt, hubOpt, coilQuorumOpt, templateOpt, outOpt)
            .mapN(generateAndWrite)

    /** Generate the key pair, print it, and perform whichever file outputs the options request. */
    private def generateAndWrite(
        roster: Option[Path],
        role: Option[Role],
        wsAddress: Option[Uri],
        hub: Option[Int],
        coilQuorum: Option[Int],
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
            registerInRoster(path, role.get, vKeyHex, wsAddress, hub, coilQuorum) *>
                IO.println(s"Registered ${role.get.toString.toLowerCase} peer in roster: $path")
        )
        _ <- (template, out).tupled.traverse_ { (templatePath, outPath) =>
            writePrivateConfig(templatePath, outPath, role.get, vKeyHex, sKeyHex) *>
                IO.println(s"Wrote private config: $outPath") *>
                writeAddressFile(outPath, address)
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
        hub: Option[Int],
        coilQuorum: Option[Int]
    ): IO[Unit] = for {
        current <- IO.blocking(Files.exists(path)).flatMap {
            case true =>
                IO.blocking(Files.readString(path))
                    .flatMap(s => IO.fromEither(parser.parse(s)))
            case false => IO.pure(emptyRoster)
        }
        updated <- IO.fromEither(
          appendPeer(current, role, vKeyHex, wsAddress, hub, coilQuorum).left
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
      "coilPeers" -> Json.arr(),
      "coilQuorum" -> Json.fromInt(0)
    )

    /** Pure roster edit: append the peer entry for its role and apply `coilQuorum` if given. */
    private[bootstrap] def appendPeer(
        roster: Json,
        role: Role,
        vKeyHex: String,
        wsAddress: Option[Uri],
        hub: Option[Int],
        coilQuorum: Option[Int]
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
        withQuorum = coilQuorum.fold(withPeer)(q => withPeer.add("coilQuorum", Json.fromInt(q)))
    } yield Json.fromJsonObject(withQuorum)

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

    /** Write the peer's L1 address next to the private config, for funding scripts. */
    private def writeAddressFile(outPath: Path, address: String): IO[Unit] = {
        val addressPath =
            Option(outPath.getParent).fold(Path.of("address.txt"))(_.resolve("address.txt"))
        IO.blocking(Files.writeString(addressPath, address + "\n")) *>
            IO.println(s"Wrote L1 address: $addressPath")
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
  *   sbt "runMain hydrozoa.bootstrap.BuildHeadConfig <bootstrap.json> \
  *     --blockfrost-key <key> --equity <lovelace> [--out head-config.json]"
  * }}}
  *
  * Reads the [[Bootstrap.BootstrapConfig]] from `<bootstrap.json>`, funds the head from head peer
  * 0's L1 address (via the Blockfrost backend), and writes the resulting [[HeadConfig]] as JSON to
  * `--out` (default `head-config.json`). Every node then loads this same artifact.
  *
  * The bootstrap config carries the `scriptReferenceUtxos` — the L1 inputs of the treasury and
  * dispute reference-script UTxOs that [[DeployReferenceScripts]] deployed. They are resolved and
  * hash-checked against [[HydrozoaBlueprint]] here, so a stale deployment fails the build rather
  * than every node's startup.
  */
object BuildHeadConfig
    extends CommandIOApp(
      name = "build-head-config",
      header = "Build the shared head-config.json artifact every node loads"
    ):

    private val logger = Logging.loggerIO("hydrozoa.bootstrap.BuildHeadConfig")

    private val bootstrapArg: Opts[Path] =
        Opts.argument[String]("bootstrap.json").map(Path.of(_))
    private val blockfrostKeyOpt: Opts[String] =
        Opts.option[String](
          "blockfrost-key",
          "Blockfrost API key for the Cardano backend (falls back to $BLOCKFROST_API_KEY)",
          short = "k"
        ).orElse(
          Opts.env[String]("BLOCKFROST_API_KEY", "Blockfrost API key for the Cardano backend")
        )
    private val equityOpt: Opts[Coin] =
        Opts.option[Long](
          "equity",
          "Head peer 0's equity contribution, in lovelace",
          short = "e"
        ).map(Coin.apply)
    private val outOpt: Opts[Path] =
        Opts.option[String]("out", "Output path (default head-config.json)", short = "o")
            .map(Path.of(_))
            .withDefault(Path.of("head-config.json"))

    override def main: Opts[IO[ExitCode]] =
        (bootstrapArg, blockfrostKeyOpt, equityOpt, outOpt).mapN(buildHeadConfig)

    private def buildHeadConfig(
        bootstrapPath: Path,
        blockfrostKey: String,
        minEquity: Coin,
        outPath: Path
    ): IO[ExitCode] =
        for {
            bootstrapStr <- IO.blocking(Files.readString(bootstrapPath))
            bootstrapConfig <- IO.fromEither(parser.decode[Bootstrap.BootstrapConfig](bootstrapStr))
            cardanoNetwork = bootstrapConfig.cardanoNetwork
            _ <- logger.info(
              s"Building shared head config: ${bootstrapConfig.headPeers.size} head peer(s), " +
                  s"${bootstrapConfig.coilPeers.size} coil peer(s), coil quorum ${bootstrapConfig.coilQuorum}"
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
              minEquity,
              scriptReferenceUtxos
            )
            _ <- IO.blocking(Files.writeString(outPath, headConfig.asJson.spaces2))
            _ <- logger.info(s"Wrote shared head config to $outPath")
        } yield ExitCode.Success

end BuildHeadConfig
