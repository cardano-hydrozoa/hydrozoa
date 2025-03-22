package hydrozoa.l1

import com.bloxbean.cardano.client.api.common.OrderEnum
import com.bloxbean.cardano.client.api.model.{Amount, ProtocolParams, Result, Utxo}
import com.bloxbean.cardano.client.backend.api.*
import com.bloxbean.cardano.client.backend.model.*
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import hydrozoa.infra.toResult
import hydrozoa.{UtxoIdL1, TxId, TxIx}

import java.io.File
import java.math.BigInteger
import java.util
import scala.jdk.CollectionConverters.*

class BackendServiceMock(cardanoL1: CardanoL1Mock) extends BackendService:
    override def getAssetService: AssetService = ???

    override def getBlockService: BlockService = BlockServiceMock(cardanoL1)

    override def getNetworkInfoService: NetworkInfoService = ???

    override def getPoolService: PoolService = ???

    override def getTransactionService: TransactionService = TransactionServiceMock(cardanoL1)

    override def getUtxoService: UtxoService = UtxoServiceMock(cardanoL1)

    override def getAddressService: AddressService = ???

    override def getAccountService: AccountService = ???

    override def getEpochService: EpochService = EpochServiceMock(cardanoL1)

    override def getMetadataService: MetadataService = ???

    override def getScriptService: ScriptService = ScriptServiceMock(cardanoL1)

class BlockServiceMock(cardanoL1Mock: CardanoL1Mock) extends BlockService:
    override def getLatestBlock: Result[Block] =
        val latest = Block.builder().slot(42).build()
        val result = Result.success("dummy").asInstanceOf[Result[Block]]
        result.withValue(latest)
        result

    override def getBlockByHash(blockHash: SubmissionError): Result[Block] = ???

    override def getBlockByNumber(blockNumber: BigInteger): Result[Block] = ???

class TransactionServiceMock(cardanoL1Mock: CardanoL1Mock) extends TransactionService:
    override def submitTransaction(cborData: Array[Byte]): Result[String] = ???

    override def getTransaction(txnHash: String): Result[TransactionContent] = ???

    override def getTransactions(
        txnHashCollection: util.List[String]
    ): Result[util.List[TransactionContent]] = ???

    override def getTransactionUtxos(txnHash: String): Result[TxContentUtxo] = ???

    override def getTransactionRedeemers(
        txnHash: String
    ): Result[util.List[TxContentRedeemers]] = ???

class UtxoServiceMock(cardanoL1Mock: CardanoL1Mock) extends UtxoService:
    override def getUtxos(address: String, count: Int, page: Int): Result[util.List[Utxo]] = ???

    override def getUtxos(
        address: String,
        count: Int,
        page: Int,
        order: OrderEnum
    ): Result[util.List[Utxo]] = ???

    override def getUtxos(
        address: String,
        unit: String,
        count: Int,
        page: Int
    ): Result[util.List[Utxo]] = ???

    override def getUtxos(
        address: String,
        unit: String,
        count: Int,
        page: Int,
        order: OrderEnum
    ): Result[util.List[Utxo]] = ???

    override def getTxOutput(txHash: String, outputIndex: Int): Result[Utxo] =
        val utxoId = UtxoIdL1(TxId(txHash), TxIx(outputIndex))
        val opt = cardanoL1Mock.utxosActive
            .get(utxoId)
            .map(output =>
                Utxo.builder()
                    .txHash(txHash)
                    .outputIndex(outputIndex)
                    .address(output.address.bech32)
                    .amount(
                      List(Amount.lovelace(BigInteger.valueOf(output.coins.longValue))).asJava
                    )
                    .build()
            )
        opt.toResult(s"utxo not found: $txHash#$outputIndex")

class EpochServiceMock(cardanoL1Mock: CardanoL1Mock) extends EpochService:
    override def getLatestEpoch: Result[EpochContent] = ???

    override def getEpoch(epoch: Integer): Result[EpochContent] = ???

    override def getProtocolParameters(epoch: Integer): Result[ProtocolParams] = ???

    override def getProtocolParameters: Result[ProtocolParams] =
        val or = new ObjectMapper().reader()
        val pp = or.readValue(File("protocolParameters.json"), classOf[ProtocolParams])
        val res = Result.success("dummy").asInstanceOf[Result[ProtocolParams]]
        res.withValue(pp)
        res

class ScriptServiceMock(cardanoL1Mock: CardanoL1Mock) extends ScriptService:
    override def getScriptDatum(datumHash: String): Result[ScriptDatum] = ???

    override def getScriptDatumCbor(datumHash: String): Result[ScriptDatumCbor] = ???

    override def getNativeScriptJson(scriptHash: String): Result[JsonNode] = ???

    override def getPlutusScriptCbor(scriptHash: String): Result[String] = ???
