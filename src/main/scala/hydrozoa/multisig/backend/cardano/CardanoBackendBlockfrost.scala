package hydrozoa.multisig.backend.cardano

import cats.data.EitherT
import cats.effect.*
import cats.effect.syntax.all.*
import cats.syntax.traverse.*
import com.bloxbean.cardano.client.api.common.OrderEnum
import com.bloxbean.cardano.client.api.model.{Result, Utxo}
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.backend.model.{AssetTransactionContent, ScriptDatumCbor, TxContentRedeemers, TxContentUtxo}
import com.bloxbean.cardano.client.plutus.spec.RedeemerTag
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.backend.cardano.CardanoBackend.Error
import hydrozoa.multisig.backend.cardano.CardanoBackend.Error.*
import io.bullet.borer.Cbor

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.{AssetName, PolicyId, ProtocolParams, Transaction, TransactionHash, TransactionInput, TransactionOutput, Utxos}
import scalus.cardano.node.BlockfrostProvider
import sttp.client4.DefaultFutureBackend

/** Cardano backend to use with Blockfrost-compatible API. Currently uses both BloxBeans's
  * [[BackendServive]] and Scalus' [[BlockfrostProvider]] for protocol parameters handle.
  *
  * @param backendService
  *   BloxBean backend service
  * @param pageSize
  *   Used when paginating over methods of [[backendService]]
  * @param blockfrostProvider
  *   Used to fulfill get protocol parameters method
  */
class CardanoBackendBlockfrost private (
    private val backendService: BackendService,
    private val pageSize: Int,
    private val blockfrostProvider: BlockfrostProvider,
) extends CardanoBackend[IO] {

    override def utxosAt(address: ShelleyAddress): IO[Either[CardanoBackend.Error, Utxos]] =
        paginate(page =>
            backendService.getUtxoService
                .getUtxos(address.toBech32.get, pageSize, page, OrderEnum.asc)
        ).map(ret => ret.map(utxos => utxos.map(convert).toMap))

    override def utxosAt(
        address: ShelleyAddress,
        asset: (PolicyId, AssetName)
    ): IO[Either[CardanoBackend.Error, Utxos]] = {
        val unit = s"${asset._1.toHex}${asset._2.bytes.toHex}"
        paginate(page =>
            backendService.getUtxoService
                .getUtxos(address.toBech32.get, unit, pageSize, page, OrderEnum.asc)
        ).map(ret => ret.map(utxos => utxos.map(convert).toMap))
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

    private def convert(utxo: Utxo): (TransactionInput, TransactionOutput) = {
        import scalus.builtin.ByteString
        import scalus.cardano.ledger.{Blake2b_256, Coin, DatumOption, Hash, HashPurpose, MultiAsset, TransactionInput, TransactionOutput, Value}

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
                    import scalus.builtin.Data
                    scala.util.Try {
                        val datumBytes = ByteString.fromHex(inlineDatumHex)
                        val data = Cbor.decode(datumBytes.bytes).to[Data].value
                        DatumOption.Inline(data): DatumOption.Inline
                    }.toOption
                }
            }

        // Script reference: Blockfrost only provides the hash, not the full script
        // We cannot reconstruct the full ScriptRef from just the hash
        // This would require a separate API call to fetch the script content
        val scriptRef = None // TODO: Fetch script from reference_script_hash if needed

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
    ): IO[Either[CardanoBackend.Error, List[(TransactionHash, Data)]]] =
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

            txRets <- txIds.traverse(txHash => EitherT(continuingInputRedeemer(txHash, unit)))

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
    private def continuingInputRedeemer(
        txHash: TransactionHash,
        unit: String
    ): IO[Either[CardanoBackend.Error, Option[(TransactionHash, Data)]]] = {
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
            _ <- EitherT.fromOption[IO](
              opt = utxos.getOutputs.asScala.find { output =>
                  output.getAmount.asScala.exists(_.getUnit == unit)
              },
              ifNone = NoTxOutputWithAsset(txHash, unit)
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

        } yield Some(txHash -> redeemer)).value.map {
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

    override def submitTx(tx: Transaction): IO[Either[CardanoBackend.Error, Unit]] =
        IO {
            val result = backendService.getTransactionService.submitTransaction(tx.toCbor)
            if result.isSuccessful
            then Right(())
            else Left(Unexpected(result.getResponse))
        }.handleError(e =>
            Left(Unexpected(s"${e.getMessage}, caused by: ${
                    if e.getCause != null then e.getCause.getMessage else "N/A"
                }"))
        )

    def latestParams: IO[Either[Error, ProtocolParams]] =
        IO { Right(Await.result(blockfrostProvider.fetchLatestParams, 10.seconds)) }
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

    def apply(
        url: Either[Network, URL],
        apiKey: ApiKey = "",
        pageSize: Int = 100,
        cardanoNetwork : CardanoNetwork.Section
    ): IO[CardanoBackendBlockfrost] = {
        val baseUrl = url.fold(_.url, x => x)
        // NB: Bloxbean requires the trailing slash
        val backendService = BFBackendService(baseUrl + "/", apiKey)

        // Scalus Blockfrost provider
        given sttp.client4.Backend[scala.concurrent.Future] = DefaultFutureBackend()

        for {
            blockfrostProvider <- IO.fromFuture(IO(
                cardanoNetwork match {
                    case CardanoNetwork.Mainnet => BlockfrostProvider.mainnet(
                        apiKey
                    )
                    case CardanoNetwork.Preprod => BlockfrostProvider.preprod(apiKey)
                    case CardanoNetwork.Preview => BlockfrostProvider.preview(apiKey)
                    case _ => ??? // @Ilia: What would you like to do here?
                }))
            //
        } yield (new CardanoBackendBlockfrost(backendService, pageSize, blockfrostProvider))

    }

    enum Network(val url: String):
        case MAINNET extends Network(Constants.BLOCKFROST_MAINNET_URL)
        case TESTNET extends Network(Constants.BLOCKFROST_TESTNET_URL)
        case PREPROD extends Network(Constants.BLOCKFROST_PREPROD_URL)
        case PREVIEW extends Network(Constants.BLOCKFROST_PREVIEW_URL)
        case SANCHONET extends Network(Constants.BLOCKFROST_SANCHONET_URL)
