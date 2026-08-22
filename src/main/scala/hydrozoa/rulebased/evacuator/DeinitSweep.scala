package hydrozoa.rulebased.evacuator

import cats.effect.{ExitCode, IO}
import cats.syntax.apply.*
import cats.syntax.contravariant.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import com.monovore.decline.{Command, Opts}
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.cardano.scalus.contextualscalus.Change
import hydrozoa.lib.cardano.scalus.contextualscalus.TransactionBuilder.{addExpectedSigners, build, finalizeContext}
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer as BackendTracer, info}
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost, CardanoBackendEventFormat}
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.consensus.peer.PeerWallet.peerWalletDecoder
import hydrozoa.multisig.ledger.l1.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx.Validators.nonSigningValidators
import hydrozoa.multisig.ledger.l1.tx.RawTx
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.TreasuryRedeemer
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.given
import io.circe.Decoder
import io.circe.parser.decode
import java.nio.file.{Files, Path}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.ScriptSource.PlutusScriptAttached
import scalus.cardano.txbuilder.ThreeArgumentPlutusScriptWitness
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, ReferenceOutput, Spend}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}

/** Closes out head generations whose evacuation has already finished.
  *
  * A resolved treasury whose `evacuationActive` is the empty-set commitment has paid out everything
  * it owed. What remains is its residual ada and its beacons — the HYDR singleton and the VOTE
  * tokens that `Resolve` absorbed — sitting at the rule-based treasury address with no one left to
  * spend them. `Deinit` is the redeemer that ends that: it burns the beacons and frees the ada.
  *
  * ★ `Deinit` needs KEYS, not an evacuation map. That is what separates these from the rest of the
  * graveyard. An unevacuated treasury can only be drained by whoever holds the map preimage, which
  * dies with the peers' stores; a fully evacuated one needs only the head's own signatures, because
  * the beacons are minted under the head's native-script policy and the validator delegates consent
  * to it.
  *
  * ★★ The validator requires the burn map to CONTAIN each spent treasury's tokens, not to equal
  * them (`RuleBasedTreasuryScript.scala`, the `Deinit` branch). So one transaction can close out
  * several generations at once: every validator instance checks its own tokens against the combined
  * mint, and each of them passes.
  *
  * Generations sharing one head policy also share one native script and one set of signatures,
  * which is what makes the batch worth building — the fee is paid once instead of per generation.
  */
object DeinitSweep {

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(
          Slf4jMsgFormat.humanFormat("hydrozoa.rulebased.evacuator.DeinitSweep")
        )

    /** The compressed BLS12-381 G1 generator, which is what committing to the empty set yields. A
      * treasury carrying it has evacuated everything.
      */
    private val emptySetCommitment: String =
        "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"

    /** One spent-out treasury as read off the chain. `tokens` is keyed by the full asset unit —
      * policy id followed by asset name — exactly as Blockfrost reports it, and `inlineDatum` is
      * the datum's CBOR, which the validator reads and so must be reproduced exactly.
      */
    final case class Dead(
        txHash: String,
        index: Int,
        lovelace: Long,
        tokens: Map[String, Long],
        inlineDatum: String
    ) derives Decoder

    private val headConfigPathArg: Opts[Path] =
        Opts.argument[String]("head-config.json").map(Path.of(_))

    private val privateConfigPathArg: Opts[Path] =
        Opts.argument[String]("peer-private.json").map(Path.of(_))

    private val keysOpt: Opts[List[Path]] =
        Opts
            .options[String]("signer", "A peer private.json whose wallet co-signs the burn")
            .map(_.toList.map(Path.of(_)))

    private val utxosOpt: Opts[Path] =
        Opts.option[String]("utxos", "JSON array of evacuated treasuries to close").map(Path.of(_))

    /** The treasury validator travels as a reference script. The utxo named in a head's config is
      * whichever one that head's bootstrap deployed, and a generation old enough to need closing
      * has usually outlived it — so the one to use is named here. Any unspent utxo carrying the
      * validator will do, since the ledger cares about the script, not about its provenance.
      */
    private val scriptRefOpt: Opts[String] =
        Opts.option[String]("treasury-script-ref", "Unspent utxo carrying the validator, txhash#ix")

    private val collateralOpt: Opts[String] =
        Opts.option[String]("collateral", "Ada-only wallet utxo to use as collateral, txhash#ix")

    private val dumpOpt: Opts[Option[Path]] =
        Opts.option[String]("dump", "Write the signed tx CBOR (hex) here").map(Path.of(_)).orNone

    private val commitOpt: Opts[Boolean] =
        Opts.flag("commit", "Actually submit; without it, build and report only").orFalse

    private val backendUrlOpt: Opts[Option[String]] =
        Opts
            .option[String]("backend-url", "Blockfrost-compatible base URL (e.g. a local Dolos)")
            .orNone

    lazy val command: Command[IO[ExitCode]] =
        Command(
          name = "sweep-deinit",
          header = "Burn the beacons of fully evacuated head generations and reclaim their ada"
        )(
          (
            headConfigPathArg,
            privateConfigPathArg,
            keysOpt,
            utxosOpt,
            scriptRefOpt,
            collateralOpt,
            commitOpt,
            backendUrlOpt,
            dumpOpt
          ).mapN(run)
        )

    def run(
        headConfigPath: Path,
        privateConfigPath: Path,
        signerPaths: List[Path],
        utxosPath: Path,
        scriptRef: String,
        collateral: String,
        commit: Boolean,
        backendUrl: Option[String],
        dump: Option[Path]
    ): IO[ExitCode] =
        for {
            override_ <- backendUrl.traverse(localBackend)
            loaded <- NodeConfig.load(headConfigPath, privateConfigPath, override_)
            (nodeConfig, backend) = loaded
            exit <- {
                given HeadConfig.Bootstrap.Section = nodeConfig.headConfig
                drive(backend, signerPaths, utxosPath, scriptRef, collateral, commit, dump)
            }
        } yield exit

    private def drive(
        backend: CardanoBackend[IO],
        signerPaths: List[Path],
        utxosPath: Path,
        scriptRef: String,
        collateral: String,
        commit: Boolean,
        dump: Option[Path]
    )(using config: HeadConfig.Bootstrap.Section): IO[ExitCode] = {

        val script = config.headMultisigScript
        val headPolicy = script.policyId

        for {
            signers <- signerPaths.traverse(loadSigner)
            _ <- IO.raiseUnless(signers.size == script.numSigners)(
              RuntimeException(
                s"multisig needs ${script.numSigners} signatures, ${signers.size} supplied"
              )
            )

            json <- IO.blocking(Files.readString(utxosPath))
            dead <- IO.fromEither(
              decode[List[Dead]](json).left.map(e => RuntimeException(s"utxos: $e"))
            )
            _ <- IO.raiseWhen(dead.isEmpty)(RuntimeException("nothing to close out"))

            // Every token of ours in these utxos is dead by definition — the generation is over.
            // Anything under another policy would be someone else's and cannot be burned here, so
            // its presence means the input was misidentified.
            foreign = dead.flatMap(_.tokens.keys).filterNot(_.startsWith(headPolicy.toHex))
            _ <- IO.raiseWhen(foreign.nonEmpty)(
              RuntimeException(s"utxos carry ${foreign.size} token(s) not under $headPolicy")
            )

            _ <- dead.traverse_(d => IO.fromEither(checkEvacuated(d)))

            refUtxo <- IO.fromEither(parseInput(scriptRef)).map(referenceUtxo)
            collateralInput <- IO.fromEither(parseInput(collateral))
            collateralUtxo <- backend
                .resolve(collateralInput)
                .flatMap(r =>
                    IO.fromEither(r.left.map(e => RuntimeException(s"collateral: $e")))
                        .flatMap(
                          IO.fromOption(_)(RuntimeException(s"collateral $collateral not found"))
                        )
                )
                .flatMap(u => IO.fromEither(CollateralUtxo.parse(u)))

            totalAda = dead.map(_.lovelace).sum
            burn = dead.flatMap(_.tokens.toList)
            _ <- log.info(s"${dead.size} evacuated generation(s), ${totalAda / 1e6} ada residual")
            _ <- log.info(s"burning ${burn.size} beacon(s) under policy $headPolicy")
            _ <- log.info(s"treasury validator referenced at $scriptRef")
            _ <- log.info(s"collateral and residual return to ${collateralUtxo.collateralOutput}")

            params <- backend.fetchLatestParams.flatMap(
              IO.fromEither(_).adaptError(e => RuntimeException(s"protocol params: $e"))
            )

            tx <- IO.fromEither(buildTx(dead, burn, refUtxo, collateralUtxo, script))
            signed = signers.foldLeft(tx)((t, w) => w.signTx(t))
            size = signed.toCbor.length
            _ <- IO.raiseWhen(size > params.maxTxSize)(
              RuntimeException(s"$size bytes exceeds the ${params.maxTxSize} limit — close fewer")
            )
            outs = signed.body.value.outputs.toList.map(_.value)
            _ <- outs.zipWithIndex.traverse_ { (o, i) =>
                log.info(f"  output $i: ${o.value.coin.value / 1e6}%.6f ada")
            }
            _ <- log.info(f"fee ${signed.body.value.fee.value / 1e6}%.6f ada")
            _ <- log.info(s"size $size bytes of ${params.maxTxSize}, tx ${signed.id}")
            _ <- dump.traverse_ { path =>
                IO.blocking(Files.writeString(path, signed.toCbor.map("%02x".format(_)).mkString))
                    .void *> log.info(s"signed tx written to $path")
            }

            exit <-
                if !commit then
                    log.info("not submitting; pass --commit to run it").as(ExitCode.Success)
                else
                    backend
                        .submitTx(RawTx(signed))
                        .flatMap(r =>
                            IO.fromEither(r.left.map(e => RuntimeException(s"submit failed: $e")))
                        ) *> log.info(s"submitted ${signed.id}").as(ExitCode.Success)
        } yield exit
    }

    /** Refuse anything that has not finished evacuating.
      *
      * The validator enforces this too, so the check buys no safety on chain — it buys a legible
      * failure. A treasury still owing payouts cannot be closed by any key, and saying so here
      * beats a phase-2 rejection that names only the input.
      */
    private def checkEvacuated(d: Dead): Either[Throwable, Unit] = {
        val datum = d.inlineDatum
        val resolved = datum.startsWith("d87a9f")
        val commitment = datum.slice(70, 70 + 96)
        if !resolved then
            Left(
              RuntimeException(s"${d.txHash}#${d.index} is unresolved — it never reached Resolve")
            )
        else if commitment != emptySetCommitment then
            Left(RuntimeException(s"${d.txHash}#${d.index} still owes payouts — evacuate it first"))
        else Right(())
    }

    /** The reference input as the ledger will see it: the validator's own bytes, at the utxo that
      * carries them. Only the script matters to phase 2, so the address and ada are the deployment
      * convention rather than anything the transaction depends on.
      */
    private def referenceUtxo(input: TransactionInput)(using
        config: HeadConfig.Bootstrap.Section
    ): Utxo =
        Utxo(
          input,
          TransactionOutput.Babbage(
            address = config.ruleBasedTreasuryAddress,
            value = Value(Coin(35_747_140L)),
            datumOption = None,
            scriptRef = Some(ScriptRef(HydrozoaBlueprint.treasuryScript))
          )
        )

    private def parseInput(s: String): Either[Throwable, TransactionInput] =
        s.split('#') match {
            case Array(h, i) if i.toIntOption.isDefined =>
                Right(TransactionInput(TransactionHash.fromHex(h), i.toInt))
            case _ => Left(RuntimeException(s"expected txhash#index, got $s"))
        }

    private def buildTx(
        dead: List[Dead],
        burn: List[(String, Long)],
        refUtxo: Utxo,
        collateralUtxo: CollateralUtxo,
        script: HeadMultisigScript
    )(using config: HeadConfig.Bootstrap.Section): Either[Throwable, Transaction] = {

        val treasuries = dead.map { d =>
            Utxo(
              TransactionInput(TransactionHash.fromHex(d.txHash), d.index),
              TransactionOutput.Babbage(
                address = config.ruleBasedTreasuryAddress,
                value = Value(Coin(d.lovelace), multiAssetOf(d.tokens)),
                datumOption = Some(DatumOption.Inline(Data.fromCbor(hexToBytes(d.inlineDatum)))),
                scriptRef = None
              )
            )
        }

        val spends = treasuries.map(u =>
            Spend(
              u,
              ThreeArgumentPlutusScriptWitness(
                PlutusScriptAttached,
                TreasuryRedeemer.Deinit.toData,
                DatumInlined
              )
            )
        )

        // The native script travels by value on the first burn that needs it and by reference
        // after; the builder's steps are not commutative, so attaching it later would leave the
        // earlier mint unable to resolve its witness.
        val burns = burn.zipWithIndex.map { case ((unit, amount), i) =>
            Mint(
              scriptHash = script.policyId,
              assetName = AssetName(ByteString.fromHex(unit.drop(56))),
              amount = -amount,
              witness = if i == 0 then script.witnessValue else script.witnessAttached
            )
        }

        val steps =
            List(ReferenceOutput(refUtxo))
                ++ spends
                ++ List(
                  collateralUtxo.spend,
                  collateralUtxo.add,
                  collateralUtxo.collateralOutput.send
                )
                ++ burns

        for {
            context <- build(steps).left
                .map(e => RuntimeException(s"deinit build failed: $e"))
            // Balancing prices the transaction it can see, and the multisig witnesses are attached
            // afterwards — without declaring them the fee is short by exactly their bytes.
            finalized <- context
                .addExpectedSigners(script.numSigners)
                .finalizeContext(
                  diffHandler = Change.changeOutputDiffHandler(0),
                  validators = nonSigningValidators
                )
                .left
                .map(e => RuntimeException(s"deinit balancing failed: $e"))
        } yield finalized.transaction
    }

    private def multiAssetOf(tokens: Map[String, Long]): MultiAsset =
        tokens.foldLeft(MultiAsset.empty) { case (acc, (unit, amount)) =>
            acc + Value
                .asset(
                  ScriptHash.fromByteString(ByteString.fromHex(unit.take(56))),
                  AssetName(ByteString.fromHex(unit.drop(56))),
                  amount
                )
                .assets
        }

    private def hexToBytes(hex: String): Array[Byte] =
        hex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

    /** Accept either a bare wallet or a peer's whole `private.json`.
      *
      * The files that hold these keys on an operator's disk are peer configs, and requiring the
      * wallet to be extracted from them first would mean copying signing keys around to satisfy a
      * parser. A head peer keeps its wallet under `ownHeadWallet` and a coil peer under
      * `ownCoilWallet`, so both are tried.
      */
    private def loadSigner(path: Path): IO[PeerWallet] =
        IO.blocking(Files.readString(path)).flatMap { s =>
            IO.fromEither(
              io.circe.parser
                  .parse(s)
                  .left
                  .map(e => RuntimeException(s"$path: $e"))
                  .flatMap { json =>
                      val root = json.hcursor
                      val nested = root.downField("ownPeerPrivate")
                      val candidates = List(
                        root,
                        nested.downField("ownHeadWallet"),
                        nested.downField("ownCoilWallet")
                      )
                      candidates
                          .flatMap(_.as[PeerWallet].toOption)
                          .headOption
                          .toRight(
                            RuntimeException(
                              s"$path: no wallet found at the root, ownPeerPrivate.ownHeadWallet " +
                                  "or ownPeerPrivate.ownCoilWallet"
                            )
                          )
                  }
            )
        }

    /** A backend pointed at a Blockfrost-compatible endpoint of our own. The network is declared
      * `Custom` purely so the base URL is ours; its parameters are still preview's, because that is
      * the chain the node is following.
      */
    private def localBackend(url: String): IO[CardanoBackend[IO]] =
        CardanoBackendBlockfrost(
          network = Right(
            (
              CardanoNetwork.Custom(
                CardanoNetwork.Preview.cardanoInfo,
                CardanoNetwork.Preview.protocolMagic
              ),
              url
            )
          ),
          apiKey = "",
          tracer = BackendTracer.sink.contramap(CardanoBackendEventFormat.humanFormat)
        ).map(b => b: CardanoBackend[IO])
}
