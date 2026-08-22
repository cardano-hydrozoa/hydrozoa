package hydrozoa.rulebased.evacuator

import cats.effect.{ExitCode, IO}
import cats.syntax.apply.*
import cats.syntax.contravariant.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import com.monovore.decline.{Command, Opts}
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.cardano.scalus.contextualscalus.TransactionBuilder.addExpectedSigners
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info}
import hydrozoa.lib.logging.Slf4jTracer as BackendTracer
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost, CardanoBackendEventFormat}
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.consensus.peer.PeerWallet.peerWalletDecoder
import hydrozoa.multisig.ledger.l1.tx.RawTx
import io.circe.Decoder
import io.circe.parser.decode
import java.nio.file.{Files, Path}
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, Send, Spend}
import scalus.cardano.txbuilder.{Change, TransactionBuilder}
import scalus.uplc.builtin.ByteString

/** Clears the abandoned state that accumulates at a head's multisig address.
  *
  * Every head generation leaves its treasury and regime utxos behind when it dies, and because the
  * peers' keys never change, every generation shares one address — so the residue accrues there
  * indefinitely. It is not merely untidy: each peer's liaison polls that address on every tick, and
  * a thousand utxos is a paginated query per tick per peer, forever.
  *
  * Two kinds of token sit in that residue, and they must be treated differently. Beacons minted
  * under the head's own policy are dead once their generation is, and the same native script that
  * guards the address is their minting policy — so they can be burned. Anything else is under a
  * policy we do not control and is not ours to destroy, so it is moved out intact.
  *
  * ★ Selection is by AGE, not by shape: the live head's own treasury and regime utxos are
  * indistinguishable from a dead generation's except that they are newer. Anything created at or
  * after the running head's initialization is left alone — which also covers deposits arriving
  * while the sweep is being prepared.
  */
object ResidueSweep {

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(
          Slf4jMsgFormat.humanFormat("hydrozoa.rulebased.evacuator.ResidueSweep")
        )

    /** One residue utxo as read off the chain. `tokens` is keyed by the full asset unit — policy id
      * followed by asset name — exactly as Blockfrost reports it.
      */
    final case class Residue(
        txHash: String,
        index: Int,
        lovelace: Long,
        tokens: Map[String, Long],
        hasScriptRef: Boolean
    ) derives Decoder

    private val headConfigPathArg: Opts[Path] =
        Opts.argument[String]("head-config.json").map(Path.of(_))

    private val privateConfigPathArg: Opts[Path] =
        Opts.argument[String]("peer-private.json").map(Path.of(_))

    private val keysOpt: Opts[List[Path]] =
        Opts
            .options[String]("signer", "A peer private.json whose wallet co-signs the sweep")
            .map(_.toList.map(Path.of(_)))

    private val utxosOpt: Opts[Path] =
        Opts.option[String]("utxos", "JSON array of residue utxos to sweep").map(Path.of(_))

    private val toOpt: Opts[String] =
        Opts.option[String]("to-key-hash", "Payment key hash of the destination (hex)")

    /** Ada to park alongside the foreign tokens, so they land in their own utxo rather than
      * diluting the consolidated ada.
      */
    private val tokenAdaOpt: Opts[Long] =
        Opts
            .option[Long]("token-output-ada", "Ada to accompany the non-head tokens")
            .withDefault(500L)
            .map(_ * 1_000_000L)

    /** Write the signed transaction out, so a rejected submission can be replayed by hand against
      * whichever endpoint is being diagnosed.
      */
    private val dumpOpt: Opts[Option[Path]] =
        Opts.option[String]("dump", "Write the signed tx CBOR (hex) here").map(Path.of(_)).orNone

    private val commitOpt: Opts[Boolean] =
        Opts.flag("commit", "Actually submit; without it, build and report only").orFalse

    /** Point the chain queries at a local node instead of the hosted API. A sweep reads every
      * residue utxo and its creating transaction, which over a hosted API is hundreds of round
      * trips at ~80 ms; against a local Dolos it is LAN latency and no request quota.
      */
    private val backendUrlOpt: Opts[Option[String]] =
        Opts
            .option[String]("backend-url", "Blockfrost-compatible base URL (e.g. a local Dolos)")
            .orNone

    lazy val command: Command[IO[ExitCode]] =
        Command(
          name = "sweep-residue",
          header = "Burn dead head beacons and move the remaining value out of a multisig address"
        )(
          (
            headConfigPathArg,
            privateConfigPathArg,
            keysOpt,
            utxosOpt,
            toOpt,
            tokenAdaOpt,
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
        toKeyHash: String,
        tokenAda: Long,
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
                drive(
                  nodeConfig,
                  backend,
                  signerPaths,
                  utxosPath,
                  toKeyHash,
                  tokenAda,
                  commit,
                  dump
                )
            }
        } yield exit

    private def drive(
        nodeConfig: NodeConfig,
        backend: CardanoBackend[IO],
        signerPaths: List[Path],
        utxosPath: Path,
        toKeyHash: String,
        tokenAda: Long,
        commit: Boolean,
        dump: Option[Path]
    )(using config: HeadConfig.Bootstrap.Section): IO[ExitCode] = {

        val script = config.headMultisigScript
        val headPolicy = script.policyId
        val multisigAddress = config.headMultisigAddress
        val destination = ShelleyAddress(
          network = config.network,
          payment = ShelleyPaymentPart.Key(AddrKeyHash(ByteString.fromHex(toKeyHash))),
          delegation = Null
        )

        for {
            signers <- signerPaths.traverse(loadSigner)
            _ <- IO.raiseUnless(signers.size == script.numSigners)(
              RuntimeException(
                s"multisig needs ${script.numSigners} signatures, ${signers.size} supplied"
              )
            )

            json <- IO.blocking(Files.readString(utxosPath))
            residue <- IO.fromEither(
              decode[List[Residue]](json).left.map(e => RuntimeException(s"utxos: $e"))
            )
            _ <- IO.raiseWhen(residue.isEmpty)(RuntimeException("nothing to sweep"))

            burn = residue
                .flatMap(_.tokens.toList)
                .filter((unit, _) => isHeadPolicy(unit, headPolicy))
            foreign = residue
                .flatMap(_.tokens.toList)
                .filterNot((unit, _) => isHeadPolicy(unit, headPolicy))
            totalAda = residue.map(_.lovelace).sum

            _ <- log.info(s"${residue.size} residue utxos, ${totalAda / 1_000_000.0} ada")
            _ <- log.info(s"burning ${burn.size} head-policy tokens (policy $headPolicy)")
            _ <- log.info(
              s"moving ${foreign.size} foreign token entries across " +
                  s"${foreign.map(_._1.take(56)).distinct.size} policies"
            )
            _ <- log.info(s"destination $destination")
            _ <- log.info(s"spending from $multisigAddress")

            params <- backend.fetchLatestParams.flatMap(
              IO.fromEither(_).adaptError(e => RuntimeException(s"protocol params: $e"))
            )

            tx <- IO.fromEither(
              build(residue, burn, foreign, multisigAddress, destination, tokenAda, params, script)
            )
            signed = signers.foldLeft(tx)((t, w) => w.signTx(t))
            size = signed.toCbor.length
            _ <- IO.raiseWhen(size > params.maxTxSize)(
              RuntimeException(s"$size bytes exceeds the ${params.maxTxSize} limit — sweep fewer")
            )
            outs = signed.body.value.outputs.toList.map(_.value)
            _ <- outs.zipWithIndex.traverse_ { (o, i) =>
                log.info(
                  f"  output $i: ${o.value.coin.value / 1e6}%.6f ada" +
                      (if o.value.assets.assets.nonEmpty then
                           s", ${o.value.assets.assets.values.map(_.size).sum} token(s)"
                       else "")
                )
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

    /** A backend pointed at a Blockfrost-compatible endpoint of our own.
      *
      * The network is declared `Custom` purely so the base URL is ours; its parameters are still
      * preview's, because that is the chain the node is following.
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

    private def isHeadPolicy(unit: String, policy: PolicyId): Boolean =
        unit.startsWith(policy.toHex)

    private def build(
        residue: List[Residue],
        burn: List[(String, Long)],
        foreign: List[(String, Long)],
        multisigAddress: ShelleyAddress,
        destination: ShelleyAddress,
        tokenAda: Long,
        params: ProtocolParams,
        script: hydrozoa.multisig.ledger.l1.script.multisig.HeadMultisigScript
    )(using config: HeadConfig.Bootstrap.Section): Either[Throwable, Transaction] = {

        // ★ Every regime utxo carries the multisig script as a REFERENCE SCRIPT, and Conway
        // collects reference scripts from spent inputs as well as reference inputs. So declaring
        // them is not cosmetic: without it the balancer misses the reference-script fee the ledger
        // charges (measured at exactly 15 lovelace per script byte), and attaching a second copy
        // of the script by value is rejected outright as an extraneous witness.
        val inputs = residue.map { r =>
            Utxo(
              TransactionInput(TransactionHash.fromHex(r.txHash), r.index),
              TransactionOutput.Babbage(
                address = multisigAddress,
                value = Value(Coin(r.lovelace), multiAssetOf(r.tokens)),
                datumOption = None,
                scriptRef = Option.when(r.hasScriptRef)(ScriptRef(script.script))
              )
            )
        }

        // The script comes from the spent utxos' own reference scripts, so nothing needs to carry
        // it by value. Only when the batch happens to hold none does a copy have to travel with
        // the first spend.
        val needsValue = !residue.exists(_.hasScriptRef)
        val spends = inputs.zipWithIndex.map { (u, i) =>
            Spend(u, if needsValue && i == 0 then script.witnessValue else script.witnessAttached)
        }

        // Burning is a negative mint under the head's own policy, which the same native script
        // authorises — so no extra witness beyond the signatures already required to spend.
        val burns = burn.map { (unit, qty) =>
            Mint(
              script.policyId,
              assetName = AssetName(ByteString.fromHex(unit.drop(56))),
              amount = -qty,
              witness = script.witnessAttached
            )
        }

        val foreignAssets = multiAssetOf(foreign.groupMapReduce(_._1)(_._2)(_ + _))

        val sends =
            List(
              // Output 0 takes the balancer's change: the bulk of the ada, ada-only.
              Send(
                TransactionOutput.Babbage(
                  address = destination,
                  value = Value(Coin(0L)),
                  datumOption = None,
                  scriptRef = None
                )
              )
            ) ++ Option
                .when(foreignAssets.assets.nonEmpty)(
                  Send(
                    TransactionOutput.Babbage(
                      address = destination,
                      value = Value(Coin(tokenAda), foreignAssets),
                      datumOption = None,
                      scriptRef = None
                    )
                  )
                )
                .toList

        for {
            ctx <- TransactionBuilder
                .build(config.network, spends ++ burns ++ sends)
                .left
                .map(e => RuntimeException(s"build failed: $e"))
            balanced <- ctx
                .addExpectedSigners(script.numSigners)
                .balanceContext(
                  diffHandler = Change.changeOutputDiffHandler(
                    _,
                    _,
                    protocolParams = params,
                    changeOutputIdx = 0
                  ),
                  protocolParams = params,
                  evaluator = PlutusScriptEvaluator(
                    config.cardanoInfo,
                    EvaluatorMode.EvaluateAndComputeCost
                  )
                )
                .left
                .map(e => RuntimeException(s"balancing failed: $e"))
        } yield balanced.transaction
    }

    private def multiAssetOf(tokens: Map[String, Long]): MultiAsset =
        tokens.toList
            .map { (unit, qty) =>
                Value
                    .asset(
                      ScriptHash.fromByteString(ByteString.fromHex(unit.take(56))),
                      AssetName(ByteString.fromHex(unit.drop(56))),
                      qty
                    )
                    .assets
            }
            .foldLeft(MultiAsset.empty)(_ + _)

    private def loadSigner(path: Path): IO[PeerWallet] =
        IO.blocking(Files.readString(path)).flatMap { s =>
            IO.fromEither(
              io.circe.parser
                  .parse(s)
                  .flatMap { j =>
                      val c = j.hcursor.downField("ownPeerPrivate")
                      c.downField("ownHeadWallet")
                          .as[PeerWallet]
                          .orElse(c.downField("ownCoilWallet").as[PeerWallet])
                  }
                  .left
                  .map(e => RuntimeException(s"$path: no peer wallet found ($e)"))
            )
        }
}
