package hydrozoa.multisig.backend.cardano

import cats.effect.IO
import com.bloxbean.cardano.client.api.common.OrderEnum
import com.bloxbean.cardano.client.api.model.{Result, Utxo}
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.quicktx.TxResult
import hydrozoa.multisig.backend.cardano.CardanoBackend.Error.Unknown
import hydrozoa.multisig.backend.cardano.CardanoBackend.GetTxInfo
import hydrozoa.{L1, OutputL1, UtxoIdL1, UtxoSet, UtxoSetL1}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.{AssetName, PolicyId, Transaction, TransactionHash}

class CardanoBackendBlockfrost private (
    private val backendService: BackendService,
    private val pageSize: Int
) extends CardanoBackend[IO] {

    override def utxosAt(address: ShelleyAddress): IO[Either[CardanoBackend.Error, UtxoSetL1]] =
        utxosAtWith(page =>
            // TODO: is it safe to use .get here?
            backendService.getUtxoService
                .getUtxos(address.toBech32.get, pageSize, page, OrderEnum.asc)
        )

    override def utxosAt(
        address: ShelleyAddress,
        asset: (PolicyId, AssetName)
    ): IO[Either[CardanoBackend.Error, UtxoSetL1]] = {
        val unit = s"${asset._1.toHex}${asset._2.bytes.toHex}"
        utxosAtWith(page =>
            // TODO: is it safe to use .get here?
            backendService.getUtxoService
                .getUtxos(address.toBech32.get, unit, pageSize, page, OrderEnum.asc)
        )
    }

    private def utxosAtWith(
        apiCall: Int => Result[java.util.List[Utxo]]
    ): IO[Either[CardanoBackend.Error, UtxoSetL1]] =
        IO {
            val utxos: mutable.Buffer[Utxo] = mutable.Buffer.empty
            var page: Int = 1

            while {
                val result = apiCall(page)
                if result.isSuccessful then {
                    result.getValue.asScala.toList match {
                        case Nil => false
                        case someUtxos =>
                            utxos.addAll(someUtxos)
                            page = page + 1
                            true
                    }
                } else {
                    throw RuntimeException(
                      s"error while trying to fetch page $page: ${result.getResponse}"
                    )
                }
            } do ()
            Right(UtxoSet[L1](utxos.map(utxo => convert(utxo)).toMap))
        }.handleError(e =>
            Left(Unknown(s"${e.getMessage}, caused by: ${
                    if e.getCause != null then e.getCause.getMessage else "N/A"
                }"))
        )

    private def convert(utxo: Utxo): (UtxoIdL1, OutputL1) = {
        import hydrozoa.{Output, UtxoId}
        import scalus.builtin.ByteString
        import scalus.cardano.ledger.{Blake2b_256, Coin, DatumOption, Hash, HashPurpose, MultiAsset, TransactionInput, TransactionOutput, Value}

        import scala.collection.immutable.SortedMap

        val txHash =
            Hash[Blake2b_256, HashPurpose.TransactionHash](ByteString.fromHex(utxo.getTxHash))
        val utxoId = UtxoId[L1](TransactionInput(txHash, utxo.getOutputIndex))

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

        val output = Output[L1](
          TransactionOutput.Babbage(
            address = address,
            value = value,
            datumOption = datumOption,
            scriptRef = scriptRef
          )
        )

        (utxoId, output)
    }

    override def getTxInfo(
        txHash: TransactionHash
    ): IO[Either[CardanoBackend.Error, GetTxInfo.Response]] =
        IO {
            val result = backendService.getTransactionService.getTransaction(txHash.toHex)
            if result.isSuccessful then Right(GetTxInfo.Response(true))
            else Right(GetTxInfo.Response(false))

        }.handleError(e =>
            Left(Unknown(s"${e.getMessage}, caused by: ${
                    if e.getCause != null then e.getCause.getMessage else "N/A"
                }"))
        )

    override def submitTx(tx: Transaction): IO[Either[CardanoBackend.Error, Unit]] =
        IO {
            backendService.getTransactionService.submitTransaction(tx.toCbor) match {
                case result: TxResult if result.isSuccessful => Right(())
                case result: TxResult                        => Left(Unknown(result.getResponse))
            }
        }.handleError(e =>
            Left(Unknown(s"${e.getMessage}, caused by: ${
                    if e.getCause != null then e.getCause.getMessage else "N/A"
                }"))
        )
}

object CardanoBackendBlockfrost:

    type URL = String
    type ApiKey = String

    def apply(
        url: Either[Network, URL],
        apiKey: ApiKey,
        pageSize: Int = 100
    ): IO[CardanoBackendBlockfrost] =
        IO {
            val backendService = BFBackendService(url.fold(_.url, x => x), apiKey)
            new CardanoBackendBlockfrost(backendService, pageSize)
        }

    enum Network(val url: String):
        case MAINNET extends Network(Constants.BLOCKFROST_MAINNET_URL)
        case TESTNET extends Network(Constants.BLOCKFROST_TESTNET_URL)
        case PREPROD extends Network(Constants.BLOCKFROST_PREPROD_URL)
        case PREVIEW extends Network(Constants.BLOCKFROST_PREVIEW_URL)
        case SANCHONET extends Network(Constants.BLOCKFROST_SANCHONET_URL)
