package hydrozoa.app

import cats.data.{NonEmptyList, NonEmptyMap, Validated}
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import com.bloxbean.cardano.client.util.HexUtil
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.coil.{CoilPeerData, CoilPeers}
import hydrozoa.config.head.initialization.{InitialBlock, InitializationParameters}
import hydrozoa.config.head.multisig.fallback.FallbackContingency.mkFallbackContingencyWithDefaults
import hydrozoa.config.head.multisig.settlement.SettlementConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.config.head.parameters.HeadParameters
import hydrozoa.config.head.peers.{HeadPeerData, HeadPeers}
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.config.node.NodeConfig
import hydrozoa.config.{HydrozoaBlueprint, ScriptReferenceUtxos}
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.logging.{ContraTracer, Logging, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info, warn}
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost, CardanoBackendEventFormat}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockHeader}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.joint.{EvacuationKey, EvacuationMap, evacuationKeyOrdering}
import hydrozoa.multisig.ledger.l1.tx.RawTx
import hydrozoa.multisig.ledger.l1.txseq.InitializationTxSeq
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.evacuationKeyToData
import io.circe.generic.semiauto.deriveDecoder
import io.circe.syntax.*
import io.circe.{Decoder, parser}
import java.nio.file.{Files, Path}
import java.security.SecureRandom
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.params.{Ed25519KeyGenerationParameters, Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}
import org.http4s.Uri
import scala.collection.immutable.{SortedMap, TreeMap}
import scalus.cardano.address.Address
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Coin, EvaluatorMode, Hash32, KeepRaw, PlutusScriptEvaluator, ScriptRef, TransactionHash, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}
import scalus.cardano.txbuilder.{Change, TransactionBuilder}
import scalus.crypto.ed25519.{SigningKey, VerificationKey}
import scalus.uplc.builtin.ByteString

object Bootstrap:

    private val logger = Logging.loggerIO("hydrozoa.app.Bootstrap")

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

    /** The static head+coil membership the build step turns into a shared [[HeadConfig]]: each head
      * peer's verification key + WebSocket address, each coil peer's verification key + hub head
      * peer, and the coil quorum. Authored once (from [[GenerateKeyPair]] output) and shared with
      * every node. Head peer 0 is the first entry in [[headPeers]].
      */
    final case class Membership(
        headPeers: List[HeadPeerData],
        coilPeers: List[CoilPeerData],
        coilQuorum: Int
    )

    object Membership {
        private given Decoder[Uri] =
            Decoder.decodeString.emap(Uri.fromString(_).left.map(_.message))
        private given Decoder[HeadPeerData] = deriveDecoder[HeadPeerData]
        given Decoder[Membership] = deriveDecoder[Membership]
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
        membership: Membership,
        minEquity: Coin
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
          coilQuorum = membership.coilQuorum,
          // Placeholder: the L2 params hash is not consumed yet. Hash32 requires 32 bytes, so use
          // a zero hash rather than empty bytes (which fail the length check).
          l2ParamsHash = Hash32.fromByteString(ByteString.fromArray(new Array[Byte](32))),
        )

        // This is the temporary hard-coded evacuation map - 10 ADA
        // goes to the fee account so the whole Sugar Rush ledger can be
        // correctly evacuated at any point of time.
        feeAccValue = Value.ada(10L)
        evacMap: EvacuationMap = EvacuationMap.apply(
          TreeMap(
            EvacuationKey
                .apply(
                  ByteString.fromHex(
                    "ef779a7b07ef490490ae0039458bccb3e78df0776dbf014e3cf780c600000000"
                  )
                )
                .get ->
                Payout.Obligation
                    .apply(
                      output = KeepRaw.apply(
                        Babbage(
                          address = Address
                              .fromBech32(
                                "addr_test1vrhh0xnmqlh5jpys4cqrj3vteje70r0swakm7q2w8nmcp3sh5wdk4"
                              ),
                          value = feeAccValue
                        )
                      ),
                      network = cardanoNetwork
                    )
                    .toOption
                    .get
          )
        )

        headPeers <- IO.fromOption(
          HeadPeers(
            NonEmptyMap.fromMapUnsafe(
              SortedMap.from(
                membership.headPeers.zipWithIndex.map((data, i) => HeadPeerNumber(i) -> data)
              )
            )
          )
        )(RuntimeException("head peers must be a non-empty list (head peer 0 first)"))

        coilPeers = CoilPeers.indexed(membership.coilPeers)
        nHeadPeers = membership.headPeers.size

        // Head peer 0 is the sole funder; its L1 enterprise address supplies the seed and funding.
        funderVKey = membership.headPeers.head.verificationKey
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
            membership.headPeers.zipWithIndex.map((_, i) =>
                HeadPeerNumber(i) -> (if i == 0 then minEquity else Coin.zero)
            )
          )
        )

        initializationParameters = InitializationParameters(
          initialEvacuationMap = evacMap,
          initialEquityContributions = initialEquityContributions,
          seedUtxo = seedUtxo,
          additionalFundingUtxos = utxosSelected.tail.map(_.toTuple).toMap,
          initialChangeOutputs = List(
            TransactionOutput.Babbage(
              address = peerAddress,
              value = valueSelected - Value(minEquity) -
                  Value(totalContingency) - feeAccValue,
              datumOption = None,
              scriptRef = None
            )
          )
        )

        bootstrap <- IO.fromEither(
          HeadConfig
              .Bootstrap(
                cardanoNetwork = cardanoNetwork,
                headParams = headParams,
                headPeers = headPeers,
                coilPeers = coilPeers,
                initializationParams = initializationParameters,
                scriptReferenceUtxos = fakeScriptReferenceUtxos(cardanoNetwork)
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
            .Build(bootstrap)(blockCreationEndTime)
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

    // TODO: remove once we have a proper way of doing things
    def fakeScriptReferenceUtxos(network: CardanoNetwork.Section): ScriptReferenceUtxos =
        val txId = TransactionHash.fromHex(
          "3ea527455ed27000badf7567912a3fec796a35accf594fa86ab974bbb7f1dad2"
        )

        val burnAddress =
            if network.network.isMainnet
            then
                Address.fromBech32(
                  "addr1wxa7ec20249sqg87yu2aqkqp735qa02q6yd93u28gzul93ghspjnt"
                )
            else
                Address.fromBech32(
                  "addr_test1wza7ec20249sqg87yu2aqkqp735qa02q6yd93u28gzul93gvc4wuw"
                )

        val treasuryScript = HydrozoaBlueprint.treasuryScript
        val disputeScript = HydrozoaBlueprint.disputeScript

        val treasuryUtxo = Utxo(
          TransactionInput(txId, 0),
          Babbage(
            burnAddress,
            Value.ada(10),
            None,
            Some(ScriptRef(treasuryScript))
          )
        )

        val disputeUtxo = Utxo(
          TransactionInput(txId, 1),
          Babbage(
            burnAddress,
            Value.ada(10),
            None,
            Some(ScriptRef(disputeScript))
          )
        )

        val treasury = ScriptReferenceUtxos
            .TreasuryScriptUtxo(network, treasuryUtxo)
            .fold(throw _, identity)
        val dispute = ScriptReferenceUtxos
            .DisputeScriptUtxo(network, disputeUtxo)
            .fold(throw _, identity)

        ScriptReferenceUtxos(
          treasury,
          dispute
        )

end Bootstrap

/** Main entry point for generating a new Ed25519 key pair.
  *
  * Prints the verification key and signing key as hex to stdout.
  */
object GenerateKeyPair extends IOApp:
    override def run(args: List[String]): IO[ExitCode] =
        Bootstrap.generateKeyPair().flatMap { case (vKey, sKey) =>
            val vKeyHex = vKey.bytes.toArray.map("%02x".format(_)).mkString
            val sKeyHex = sKey.bytes.toArray.map("%02x".format(_)).mkString
            for {
                _ <- IO.println("Generated new Ed25519 key pair:")
                _ <- IO.println(s"Verification key (32 bytes): $vKeyHex")
                _ <- IO.println(s"Signing key (32 bytes): $sKeyHex")
                _ <- IO.println(
                  s"Testnet address: ${vKey.shelleyAddress()(using CardanoNetwork.Preview).toBech32.get}"
                )
            } yield ExitCode.Success
        }
end GenerateKeyPair

/** Migrate all funds from the configured peer's address to a destination address.
  *
  * Usage:
  * {{{
  *   sbt "runMain hydrozoa.app.Migrate <head-config.json> <peer-private.json> <bech32-dest>"
  * }}}
  *
  * Reads wallet + Blockfrost credentials from the same files Main does; signs with the peer wallet
  * carried by the private config.
  */
object Migrate
    extends CommandIOApp(
      name = "migrate",
      header = "Send all UTXOs at the peer's wallet address to a destination address"
    ):

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("hydrozoa.app.Migrate"))

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
  *   sbt "runMain hydrozoa.app.BuildHeadConfig <peers.json> \
  *     --blockfrost-key <key> --equity <lovelace> [--out head-config.json]"
  * }}}
  *
  * Reads the head+coil membership from `<peers.json>`, funds the head from head peer 0's L1 address
  * (via the Blockfrost backend), and writes the resulting [[HeadConfig]] as JSON to `--out`
  * (default `head-config.json`). Every node then loads this same artifact.
  */
object BuildHeadConfig
    extends CommandIOApp(
      name = "build-head-config",
      header = "Build the shared head-config.json artifact every node loads"
    ):

    private val logger = Logging.loggerIO("hydrozoa.app.BuildHeadConfig")

    private val cardanoNetwork: StandardCardanoNetwork = CardanoNetwork.Preview

    private val membershipArg: Opts[Path] =
        Opts.argument[String]("peers.json").map(Path.of(_))
    private val blockfrostKeyOpt: Opts[String] =
        Opts.option[String](
          "blockfrost-key",
          "Blockfrost API key for the Cardano backend",
          short = "k"
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
        (membershipArg, blockfrostKeyOpt, equityOpt, outOpt).mapN(buildHeadConfig)

    private def buildHeadConfig(
        membershipPath: Path,
        blockfrostKey: String,
        minEquity: Coin,
        outPath: Path
    ): IO[ExitCode] =
        for {
            membershipStr <- IO.blocking(Files.readString(membershipPath))
            membership <- IO.fromEither(parser.decode[Bootstrap.Membership](membershipStr))
            _ <- logger.info(
              s"Building shared head config: ${membership.headPeers.size} head peer(s), " +
                  s"${membership.coilPeers.size} coil peer(s), coil quorum ${membership.coilQuorum}"
            )
            backend <- CardanoBackendBlockfrost(
              Left(cardanoNetwork),
              blockfrostKey,
              tracer = Slf4jTracer.sink.contramap(CardanoBackendEventFormat.humanFormat)
            )
            headConfig <- Bootstrap.mkSharedHeadConfig(cardanoNetwork, backend)(
              membership,
              minEquity
            )
            _ <- IO.blocking(Files.writeString(outPath, headConfig.asJson.spaces2))
            _ <- logger.info(s"Wrote shared head config to $outPath")
        } yield ExitCode.Success

end BuildHeadConfig
