package hydrozoa.multisig.backend.cardano

import cats.data.EitherT
import cats.effect.*
import cats.syntax.traverse.*
import com.bloxbean.cardano.client.api.common.OrderEnum
import com.bloxbean.cardano.client.api.model.{Result, Utxo}
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.backend.model.{AssetTransactionContent, ScriptDatumCbor, TxContentRedeemers, TxContentUtxo}
import com.bloxbean.cardano.client.plutus.spec.RedeemerTag
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.backend.cardano.CardanoBackend.Error
import hydrozoa.multisig.backend.cardano.CardanoBackend.Error.*
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx
import io.bullet.borer.Cbor
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.util.Try
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger
import scalus.cardano.ledger.*
import scalus.cardano.node.{BlockfrostProvider, BlockfrostProviderPlatform}
import scalus.uplc.builtin.{ByteString, Data}
import sttp.client4.Backend

/** Cardano backend to use with Blockfrost-compatible API. Currently, uses both BloxBeans's
  * [[BackendServive]] and Scalus' [[BlockfrostProvider]] for protocol parameters handle.
  *
  * @param backendService
  *   BloxBean backend service
  * @param pageSize
  *   Used when paginating over methods of [[backendService]]
  * @param blockfrostProviderFuture
  *   Used to fulfill get protocol parameters method. We keep it as Future till the time we use it
  *   to maintain semantic we had before Scalus refactored [[BlockfrostProvider]], we want to deffer
  *   all errors till the time we actualy use the provider.
  */

class CardanoBackendBlockfrost private (
    private val backendService: BackendService,
    private val pageSize: Int,
    private val blockfrostProviderFuture: Future[BlockfrostProvider],
    protected val tracer: ContraTracer[IO, CardanoBackendEvent]
) extends CardanoBackend[IO] {

    override def resolve(input: Input): IO[Either[Error, Option[ledger.Utxo]]] =
        (for {
            res <- EitherT.fromEither[IO](
              Try(
                backendService.getUtxoService.getTxOutput(input.transactionId.toHex, input.index)
              ).toEither.left.map(e => Error.ErrorResolving(input, e.getMessage))
            )
            mbUtxo <- res match {
                // Resolution "successful", but no utxo found
                case _ if res.code() == 404 => EitherT.right(IO.pure(None))
                // Resolution genuinely successful: utxo was found
                case _ if res.isSuccessful =>
                    for {
                        utxos <- EitherT(convertUtxosWithScripts(List(res.getValue)))
                        utxo = ledger.Utxo(utxos.head)
                    } yield Some(utxo)
                // Resolution unsuccessful for some other reason
                case _ =>
                    EitherT.left(
                      IO.pure(ErrorResolving(input, s"resolution response: ${res.getResponse}"))
                    )
            }
        } yield mbUtxo).value

    override def utxosAt(address: ShelleyAddress): IO[Either[CardanoBackend.Error, Utxos]] =
        paginate(page =>
            backendService.getUtxoService
                .getUtxos(address.toBech32.get, pageSize, page, OrderEnum.asc)
        ).map(_.map(convertUtxosWithoutScripts))

    override def utxosAt(
        address: ShelleyAddress,
        asset: (PolicyId, AssetName)
    ): IO[Either[CardanoBackend.Error, Utxos]] = {
        val unit = s"${asset._1.toHex}${asset._2.bytes.toHex}"
        paginate(page =>
            backendService.getUtxoService
                .getUtxos(address.toBech32.get, unit, pageSize, page, OrderEnum.asc)
        ).map(_.map(convertUtxosWithoutScripts))
    }

    /** Converts UTXOs without fetching their reference scripts: any `scriptRef` on the resulting
      * outputs is left as `None`.
      *
      * Used by [[utxosAt]]. None of its callers read the reference script off the returned UTXOs
      * (they use only value/datum/input), so fetching scripts here would be pure waste — one extra
      * Blockfrost /scripts request per ref-script UTXO, on a hot polling path. The one consumer
      * that genuinely needs the script (the reference-script checker in
      * `ScriptReferenceUtxos.resolve`) goes through [[resolve]], which keeps
      * [[convertUtxosWithScripts]].
      */
    private def convertUtxosWithoutScripts(utxos: List[Utxo]): Utxos =
        utxos.map(convert(_, scriptRef = None)).toMap

    /** Converts UTXOs with script fetching. Fetches reference scripts if needed before converting.
      *
      * Only used by [[resolve]], which backs the reference-script checker; the polling [[utxosAt]]
      * path deliberately avoids the per-UTXO /scripts request via [[convertUtxosWithoutScripts]].
      */
    private def convertUtxosWithScripts(
        utxos: List[Utxo]
    ): IO[Either[CardanoBackend.Error, Utxos]] = {
        utxos
            .traverse { utxo =>
                val scriptRefEither
                    : IO[Either[CardanoBackend.Error, Option[scalus.cardano.ledger.ScriptRef]]] =
                    Option(utxo.getReferenceScriptHash) match {
                        case None => IO.pure(Right(None))
                        case Some(scriptHash) =>
                            IO.delay(fetchScript(scriptHash)).flatMap {
                                case Left(error) =>
                                    val utxoId = TransactionInput(
                                      Hash[Blake2b_256, HashPurpose.TransactionHash](
                                        ByteString.fromHex(utxo.getTxHash)
                                      ),
                                      utxo.getOutputIndex
                                    )
                                    tracer
                                        .traceWith(
                                          CardanoBackendEvent.FetchScriptFailed(utxoId, error)
                                        )
                                        .as(Left(error))
                                case Right(script) =>
                                    IO.pure(Right(Some(scalus.cardano.ledger.ScriptRef(script))))
                            }
                    }

                scriptRefEither.map {
                    case Left(error)      => Left(error)
                    case Right(scriptRef) => Right(convert(utxo, scriptRef))
                }
            }
            .map { results =>
                results.sequence.map(_.toMap)
            }
    }

    private def paginate[A](
        apiCall: Int => Result[java.util.List[A]],
        mbStopPred: Option[A => Boolean] = None
    ): IO[Either[CardanoBackend.Error, List[A]]] =
        IO {
            val elems: mutable.Buffer[A] = mutable.Buffer.empty
            var page: Int = 1

            while {
                val result = apiCall(page)
                if result.isSuccessful then {
                    result.getValue.asScala.toList match {
                        case Nil => false
                        case someElems =>
                            val toAdd = mbStopPred.fold(someElems)(stopPred =>
                                someElems.takeWhile(e => !stopPred(e))
                            )
                            elems.addAll(toAdd)
                            page = page + 1
                            toAdd.sizeIs == someElems.size
                    }
                } else {
                    // Blockfrost replies with HTTP 404 when there is no elements on the list
                    if result.code() == 404
                    then false
                    else
                        throw RuntimeException(
                          s"Non-404 error while trying to fetch page $page: ${result.getResponse}"
                        )
                }
            } do ()
            Right(elems.toList)
        }.handleError(e =>
            Left(Unexpected(s"${e.getMessage}, caused by: ${
                    if e.getCause != null then e.getCause.getMessage else "N/A"
                }"))
        )

    /** Fetches a script by its hash from Blockfrost, trying both native and Plutus scripts. Returns
      * the script as a Scalus Script type (Native or PlutusV3).
      *
      * Uses lazy evaluation to try native script first, then Plutus if native fails.
      */
    private def fetchScript(
        scriptHash: String
    ): Either[CardanoBackend.Error, scalus.cardano.ledger.Script] = {

        lazy val nativeResult: Either[String, scalus.cardano.ledger.Script] =
            Try(backendService.getScriptService.getNativeScript(scriptHash)).toEither.left
                .map(ex => s"Exception fetching native script $scriptHash: ${ex.getMessage}")
                .flatMap { res =>
                    if res.isSuccessful then
                        convertNativeScript(res.getValue)
                            .toRight(
                              s"Failed to convert native script $scriptHash: conversion returned None"
                            )
                    else Left(s"Failed to fetch native script $scriptHash: ${res.getResponse}")
                }

        lazy val plutusResult: Either[String, scalus.cardano.ledger.Script] =
            Try(backendService.getScriptService.getPlutusScript(scriptHash)).toEither.left
                .map(ex => s"Exception fetching Plutus script $scriptHash: ${ex.getMessage}")
                .flatMap { res =>
                    if res.isSuccessful then
                        convertPlutusScript(res.getValue)
                            .toRight(
                              s"Failed to convert Plutus script $scriptHash: conversion returned None"
                            )
                    else Left(s"Failed to fetch Plutus script $scriptHash: ${res.getResponse}")
                }

        // Alternative-like behavior: try native first, fallback to Plutus if it fails
        nativeResult
            .orElse(plutusResult)
            .left
            .map(errorMsg => Unexpected(s"Failed to fetch script with hash $scriptHash: $errorMsg"))
    }

    /** Converts a BloxBean NativeScript to a Scalus Script.Native.
      *
      * @return
      *   Some(script) if conversion succeeds, None if it fails
      */
    private def convertNativeScript(
        native: com.bloxbean.cardano.client.transaction.spec.script.NativeScript
    ): Option[scalus.cardano.ledger.Script.Native] = {
        scala.util.Try {
            val scriptBytes = native.serializeScriptBody()
            // Parse the native script CBOR to get the Timelock
            import io.bullet.borer.Cbor
            import scalus.cardano.ledger.Timelock
            val timelock = Cbor.decode(scriptBytes).to[Timelock].value
            scalus.cardano.ledger.Script.Native(timelock)
        }.toOption
    }

    /** Converts a BloxBean PlutusScript to a Scalus Script (PlutusV1, PlutusV2, or PlutusV3).
      *
      * @return
      *   Some(script) if conversion succeeds, None if it fails or version is unsupported
      */
    private def convertPlutusScript(
        plutus: com.bloxbean.cardano.client.plutus.spec.PlutusScript
    ): Option[scalus.cardano.ledger.Script] = {
        import com.bloxbean.cardano.client.plutus.spec.Language

        scala.util.Try {
            val scriptBytes = ByteString.fromArray(plutus.serializeScriptBody())
            plutus.getLanguage match {
                case Language.PLUTUS_V1 => scalus.cardano.ledger.Script.PlutusV1(scriptBytes)
                case Language.PLUTUS_V2 => scalus.cardano.ledger.Script.PlutusV2(scriptBytes)
                case Language.PLUTUS_V3 => scalus.cardano.ledger.Script.PlutusV3(scriptBytes)
            }
        }.toOption
    }

    /** Pure function to convert a BloxBean UTXO to Scalus types.
      *
      * @param utxo
      *   The BloxBean UTXO
      * @param scriptRef
      *   Optional reference script (already fetched)
      * @return
      *   A pair of TransactionInput and TransactionOutput
      */
    private def convert(
        utxo: Utxo,
        scriptRef: Option[scalus.cardano.ledger.ScriptRef]
    ): (TransactionInput, TransactionOutput) = {
        import scalus.cardano.ledger.{Blake2b_256, Coin, DatumOption, Hash, HashPurpose, MultiAsset, TransactionInput, TransactionOutput, Value}
        import scalus.uplc.builtin.ByteString

        import scala.collection.immutable.SortedMap

        val txHash =
            Hash[Blake2b_256, HashPurpose.TransactionHash](ByteString.fromHex(utxo.getTxHash))
        val utxoId = TransactionInput(txHash, utxo.getOutputIndex)

        // Parse address from bech32
        val address: Address = Address.fromBech32(utxo.getAddress) match {
            case addr: scalus.cardano.address.ShelleyAddress => addr
            case _ =>
                throw new IllegalArgumentException(s"Unsupported address type: ${utxo.getAddress}")
        }

        // Convert amounts to Value (lovelace + MultiAsset)
        val amounts = utxo.getAmount.asScala.toList
        val lovelace = amounts.find(_.getUnit == "lovelace").fold(0L)(_.getQuantity.longValue)

        // Build MultiAsset from non-lovelace amounts
        val assetsByPolicy = amounts.filter(_.getUnit != "lovelace").groupBy { amount =>
            // Unit format: policyId + assetName (both hex concatenated)
            val unit = amount.getUnit
            scalus.cardano.ledger.ScriptHash.fromByteString(
              ByteString.fromHex(unit.take(56))
            ): PolicyId // First 56 chars = 28 bytes = policy ID
        }

        val assets = {
            import scalus.cardano.ledger.Hash.given
            SortedMap.from(assetsByPolicy.map { case (policyId, assetList) =>
                val assetMap = SortedMap.from(assetList.map { amount =>
                    val unit = amount.getUnit
                    val assetNameHex = unit.drop(56) // Remaining chars = asset name
                    val assetName = AssetName(ByteString.fromHex(assetNameHex))
                    (assetName, amount.getQuantity.longValue)
                })
                (policyId, assetMap)
            })
        }

        val value = Value(Coin(lovelace), MultiAsset(assets))

        // Parse datum if present - inline datum is CBOR-encoded Data in hex
        val datumOption: Option[DatumOption.Inline] =
            Option(utxo.getInlineDatum).flatMap { inlineDatumHex =>
                if inlineDatumHex.isEmpty then None
                else {
                    import io.bullet.borer.Cbor
                    import scalus.uplc.builtin.Data
                    scala.util.Try {
                        val datumBytes = ByteString.fromHex(inlineDatumHex)
                        val data = Cbor.decode(datumBytes.bytes).to[Data].value
                        DatumOption.Inline(data): DatumOption.Inline
                    }.toOption
                }
            }

        val output =
            TransactionOutput.Babbage(
              address = address,
              value = value,
              datumOption = datumOption,
              scriptRef = scriptRef
            )

        (utxoId, output)
    }

    override def isTxKnown(
        txHash: TransactionHash
    ): IO[Either[CardanoBackend.Error, Boolean]] =
        IO {
            val result = backendService.getTransactionService.getTransaction(txHash.toHex)
            if result.isSuccessful then {
                Right(true)
            } else {
                // Blockfrost replies with HTTP 404 when there is no such transaction
                if result.code() == 404
                then Right(false)
                else
                    throw RuntimeException(
                      s"Non-404 error while trying to call Blockfrost ${result.getResponse}"
                    )
            }
        }.handleError(e =>
            Left(Unexpected(s"${e.getMessage}, caused by: ${
                    if e.getCause != null then e.getCause.getMessage else "N/A"
                }"))
        )

    override def lastContinuingTxs(
        asset: (PolicyId, AssetName),
        after: TransactionHash
    ): IO[Either[CardanoBackend.Error, List[(TransactionHash, Data, Data)]]] =
        val unit = s"${asset._1.toHex}${asset._2.bytes.toHex}"
        val hex = after.toHex
        (for {
            txIds <- EitherT(
              paginate(
                apiCall = page =>
                    backendService.getAssetService.getTransactions(
                      unit,
                      pageSize,
                      page,
                      OrderEnum.desc
                    ),
                mbStopPred = Some((c: AssetTransactionContent) => {
                    c.getTxHash == hex
                })
              ).map(ret =>
                  ret.map(content => content.map(e => TransactionHash.fromHex(e.getTxHash)))
              )
            )

            txRets <- txIds.traverse(txHash =>
                EitherT(continuingInputRedeemerAndOutputDatum(txHash, unit))
            )

        } yield txRets.flatten).value

    /** Tries to treat a transaction as one having a continue output with the asset. Returns the tx
      * hash -> the redeemer of the continuing input if a tx is good. Returns None if tx doesn't
      * conform the pattern, i.e., an input is missing, an output is missing or the redeemer is
      * missing. NB: Decoding redeemer error is thrown though.
      *
      * @param txHash
      * @param unit
      *   the asset unit string (policyId + assetName hex)
      * @return
      */
    private def continuingInputRedeemerAndOutputDatum(
        txHash: TransactionHash,
        unit: String
    ): IO[Either[CardanoBackend.Error, Option[(TransactionHash, Data, Data)]]] = {
        (for {
            utxos <- EitherT(txUtxos(txHash))
            inputIx <- EitherT.fromOption[IO](
              opt = utxos.getInputs.asScala.zipWithIndex
                  .find { (input, _) =>
                      input.getAmount.asScala.exists(_.getUnit == unit)
                  }
                  .map(_._2),
              ifNone = NoTxInputWithAsset(txHash, unit)
            )
            output <- EitherT.fromOption[IO](
              opt = utxos.getOutputs.asScala.find { output =>
                  output.getAmount.asScala.exists(_.getUnit == unit)
              },
              ifNone = NoTxOutputWithAsset(txHash, unit)
            )
            outputDatum <- EitherT.fromOption[IO](
              opt = scala.util.Try {
                  val datumBytes =
                      ByteString.fromHex(output.getInlineDatum)
                  Cbor.decode(datumBytes.bytes).to[Data].value
              }.toOption,
              ifNone = ErrorDecodingDatumCbor(output.getInlineDatum)
            )

            redeemerInfo <- EitherT(txRedeemer(txHash, inputIx))

            redeemerData <- EitherT(redeemerByHash(redeemerInfo.getDatumHash))

            redeemer <- EitherT.fromOption[IO](
              opt = scala.util.Try {
                  val datumBytes =
                      ByteString.fromHex(redeemerData.getCbor)
                  Cbor.decode(datumBytes.bytes).to[Data].value
              }.toOption,
              ifNone = ErrorDecodingRedeemerCbor(redeemerData.getCbor)
            )

        } yield Some(txHash, redeemer, outputDatum)).value.map {
            // Some errors are ignored - there may be txs that doesn't conform
            // the pattern.
            case Left(NoTxInputWithAsset(_, _))       => Right(None)
            case Left(NoTxOutputWithAsset(_, _))      => Right(None)
            case Left(SpendingRedeemerNotFound(_, _)) => Right(None)
            case other                                => other
        }
    }

    private def txUtxos(txHash: TransactionHash): IO[Either[CardanoBackend.Error, TxContentUtxo]] =
        IO.delay(backendService.getTransactionService.getTransactionUtxos(txHash.toHex))
            .map(res =>
                if res.isSuccessful then Right(res.getValue)
                else
                    Left(
                      Unexpected(
                        s"Unexpected exception while retrieving tx utxos: ${res.getResponse}"
                      )
                    )
            )
            .handleError(e =>
                Left(
                  Unexpected(
                    s"Unexpected exception while retrieving tx utxos: ${e.getMessage}, caused by: ${
                            if e.getCause != null then e.getCause.getMessage else "N/A"
                        }"
                  )
                )
            )

    private def txRedeemer(
        txHash: TransactionHash,
        inputIx: Int
    ): IO[Either[CardanoBackend.Error, TxContentRedeemers]] =
        IO.delay(backendService.getTransactionService.getTransactionRedeemers(txHash.toHex))
            .map(res =>
                if res.isSuccessful
                then
                    res.getValue.asScala.toList
                        .find(r => r.getTxIndex == inputIx && r.getPurpose == RedeemerTag.Spend)
                        .toRight(SpendingRedeemerNotFound(txHash, inputIx))
                else
                    Left(
                      Unexpected(
                        s"Unexpected exception while retrieving tx redeemers: ${res.getResponse}"
                      )
                    )
            )
            .handleError(e =>
                Left(
                  Unexpected(
                    s"Unexpected exception while retrieving tx redeemers: ${e.getMessage}, caused by: ${
                            if e.getCause != null then e.getCause.getMessage else "N/A"
                        }"
                  )
                )
            )

    private def redeemerByHash(
        redeemerHash: String
    ): IO[Either[CardanoBackend.Error, ScriptDatumCbor]] =
        IO.delay(
          backendService.getScriptService
              .getScriptDatumCbor(redeemerHash)
        ).map(res =>
            if res.isSuccessful then Right(res.getValue)
            else
                Left(
                  Unexpected(
                    s"Unexpected exception while retrieving redeemer by its hash: ${res.getResponse}"
                  )
                )
        ).handleError(e =>
            Left(
              Unexpected(
                s"Unexpected exception while retrieving redeemer by its hash: ${e.getMessage}, caused by: ${
                        if e.getCause != null then e.getCause.getMessage else "N/A"
                    }"
              )
            )
        )

    override def submitTx(etx: EnrichedTx[?]): IO[Either[CardanoBackend.Error, Unit]] =
        IO {
            val result = backendService.getTransactionService.submitTransaction(etx.tx.toCbor)
            if result.isSuccessful
            then Right(())
            else Left(Unexpected(result.getResponse))
        }.handleError(e =>
            Left(Unexpected(s"${e.getMessage}, caused by: ${
                    if e.getCause != null then e.getCause.getMessage else "N/A"
                }"))
        )

    override def fetchLatestParams: IO[Either[Error, ProtocolParams]] =
        (for
            provider <- IO.fromFuture(IO.pure(blockfrostProviderFuture))
            result <- IO.fromFuture(IO.pure(provider.fetchLatestParams))
        yield Right(result))
            .handleError(e =>
                Left(Unexpected(s"${e.getMessage}, caused by: ${
                        if e.getCause != null then e.getCause.getMessage else "N/A"
                    }"))
            )

    def getStartupParams: IO[Either[Error, ProtocolParams]] =
        (for provider <- IO.fromFuture(IO.pure(blockfrostProviderFuture))
        yield Right(provider.cardanoInfo.protocolParams))
            .handleError(e =>
                Left(Unexpected(s"${e.getMessage}, caused by: ${
                        if e.getCause != null then e.getCause.getMessage else "N/A"
                    }"))
            )

}

object CardanoBackendBlockfrost:

    // TODO: use uri from sttp?
    type URL = String
    type ApiKey = String

    def apply_(
        network: Either[StandardCardanoNetwork, (CardanoNetwork.Custom, URL)],
        apiKey: ApiKey = "",
        pageSize: Int = 100,
        tracer: ContraTracer[IO, CardanoBackendEvent]
    ): CardanoBackendBlockfrost = {
        // 1. BloxBean service
        // Strip any trailing slash so we never emit `…/api/v1//path`; Bloxbean wants exactly one.
        val baseUrl = network.fold(_.baseUrl, _._2).stripSuffix("/")
        val backendService = BFBackendService(s"$baseUrl/", apiKey)

        // 2. Scalus blockfrost provider
        val blockfrostProviderFuture =
            network match {
                case Left(std) =>
                    std match {
                        case CardanoNetwork.Mainnet =>
                            BlockfrostProvider.mainnet(apiKey)
                        case CardanoNetwork.Preprod =>
                            BlockfrostProvider.preprod(apiKey)
                        case CardanoNetwork.Preview =>
                            BlockfrostProvider.preview(apiKey)

                    }
                case Right(custom, customBaseUrl) =>
                    // Reuse the already-resolved CardanoInfo (params + network + slot config) instead
                    // of re-fetching params via `create`, so the backend cannot diverge from the
                    // CardanoInfo pinned into the head-config, and one HTTP round-trip is saved.
                    given Backend[Future] = BlockfrostProviderPlatform.defaultBackend
                    Future.successful(
                      new BlockfrostProvider(
                        apiKey,
                        customBaseUrl.stripSuffix("/"),
                        5,
                        custom.cardanoInfo
                      )
                    )
            }

        new CardanoBackendBlockfrost(
          backendService,
          pageSize,
          blockfrostProviderFuture,
          tracer
        )
    }

    def apply(
        network: Either[StandardCardanoNetwork, (CardanoNetwork.Custom, URL)],
        apiKey: ApiKey = "",
        pageSize: Int = 100,
        tracer: ContraTracer[IO, CardanoBackendEvent]
    ): IO[CardanoBackendBlockfrost] =
        IO.delay(apply_(network, apiKey, pageSize, tracer))

    /** Build a backend for a [[CardanoNetwork]] directly: a standard network derives its own
      * Blockfrost URL; a `Custom` one uses `blockfrostApiUrl` (failing if it is absent). Folds the
      * network→selector resolution so callers need not thread the internal `Either` selector.
      */
    def apply(
        network: CardanoNetwork,
        blockfrostApiUrl: Option[URL],
        apiKey: ApiKey,
        tracer: ContraTracer[IO, CardanoBackendEvent]
    ): IO[CardanoBackendBlockfrost] =
        networkSelector(network, blockfrostApiUrl).flatMap(selector =>
            apply(selector, apiKey, tracer = tracer)
        )

    /** Resolve a [[CardanoNetwork]] + optional `blockfrostApiUrl` into the selector [[apply]]
      * expects. A standard network with no URL uses its own public Blockfrost endpoint; a standard
      * network *with* a URL is served from that private endpoint while keeping its baked-in
      * `CardanoInfo` (modeled as a `Custom` over the standard `CardanoInfo`, so params/slot/magic
      * are never re-fetched). A `Custom` network requires a URL and fails without one.
      */
    private[cardano] def networkSelector(
        network: CardanoNetwork,
        blockfrostApiUrl: Option[URL]
    ): IO[Either[StandardCardanoNetwork, (CardanoNetwork.Custom, URL)]] =
        network match {
            case standard: StandardCardanoNetwork =>
                blockfrostApiUrl match {
                    case None      => IO.pure(Left(standard))
                    case Some(url) =>
                        // A standard chain served by a private Blockfrost endpoint: keep the
                        // baked-in CardanoInfo (correct params/slot/magic — never fetched) but send
                        // queries and submissions to `url`.
                        val custom: CardanoNetwork.Custom =
                            CardanoNetwork.Custom(standard.cardanoInfo, standard.protocolMagic)
                        IO.pure(Right((custom, url)))
                }
            case custom: CardanoNetwork.Custom =>
                blockfrostApiUrl match {
                    case Some(url) => IO.pure(Right((custom, url)))
                    case None =>
                        IO.raiseError(
                          IllegalStateException(
                            "a Custom cardanoNetwork requires blockfrostApiUrl in the peer's " +
                                "private config"
                          )
                        )
                }
        }

    extension (self: StandardCardanoNetwork)
        def baseUrl: URL = self match {
            case _: CardanoNetwork.Mainnet.type => BlockfrostProvider.mainnetUrl
            case _: CardanoNetwork.Preprod.type => BlockfrostProvider.preprodUrl
            case _: CardanoNetwork.Preview.type => BlockfrostProvider.previewUrl
        }
