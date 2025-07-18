package hydrozoa.l1

import com.bloxbean.cardano.client.api.common.OrderEnum
import com.bloxbean.cardano.client.api.model.{Amount, ProtocolParams, Result, Utxo}
import com.bloxbean.cardano.client.api.util.AssetUtil
import com.bloxbean.cardano.client.backend.api.*
import com.bloxbean.cardano.client.backend.model.*
import com.fasterxml.jackson.databind.JsonNode
import hydrozoa.*
import hydrozoa.infra.{ResultUtils, toResult}

import java.math.BigInteger
import java.util
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class BackendServiceMock(cardanoL1: CardanoL1Mock, pp: ProtocolParams) extends BackendService:
    override def getAssetService: AssetService = ???

    override def getBlockService: BlockService = new BlockServiceMock

    override def getNetworkInfoService: NetworkInfoService = ???

    override def getPoolService: PoolService = ???

    override def getTransactionService: TransactionService = new TransactionServiceMock

    override def getUtxoService: UtxoService = UtxoServiceMock(cardanoL1)

    override def getAddressService: AddressService = ???

    override def getAccountService: AccountService = ???

    override def getEpochService: EpochService = EpochServiceMock(pp)

    override def getMetadataService: MetadataService = ???

    override def getScriptService: ScriptService = new ScriptServiceMock

class BlockServiceMock extends BlockService:
    override def getLatestBlock: Result[Block] =
        val latest = Block.builder().slot(42).build()
        val result = Result.success("dummy").asInstanceOf[Result[Block]]
        result.withValue(latest)
        result

    override def getBlockByHash(blockHash: SubmissionError): Result[Block] = ???

    override def getBlockByNumber(blockNumber: BigInteger): Result[Block] = ???

class TransactionServiceMock extends TransactionService:
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
        _order: OrderEnum
    ): Result[util.List[Utxo]] =
        val addressUtxos = cardanoL1Mock.getUtxosActive
            .filter((_, output) => output.address == AddressBech[L1](address))
            .map((id, output) => mkUtxo(id.txId.hash, id.outputIx.ix)(output))

        addressUtxos.drop(count * (page - 1)).take(count) match
            case Nil       => ResultUtils.mkResultError
            case pageElems => ResultUtils.mkResult(pageElems.toList.asJava)

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
        val utxoId = UtxoIdL1(TxId(txHash), TxIx(outputIndex.toChar))
        val opt = cardanoL1Mock.getUtxosActive
            .get(utxoId)
            .map(mkUtxo(txHash, outputIndex))
        opt.toResult(s"utxo not found: $txHash#$outputIndex")

    def mkUtxo(txHash: String, outputIndex: Int)(output: OutputL1): Utxo =

        val amounts: mutable.Set[Amount] = mutable.Set.empty

        output.tokens.foreach((policyId, tokens) =>
            tokens.foreach((tokenName, quantity) =>
                val unit = AssetUtil.getUnit(policyId.policyId, tokenName.tokenNameHex)
                amounts.add(Amount.asset(unit, quantity.longValue))
            )
        )

        Utxo
            .builder()
            .txHash(txHash)
            .outputIndex(outputIndex)
            .address(output.address.bech32)
            .amount(
              (amounts.toSet + Amount.lovelace(
                BigInteger.valueOf(output.coins.longValue)
              )).toList.asJava
            )
            .build()

class EpochServiceMock(pp: ProtocolParams) extends EpochService:
    override def getLatestEpoch: Result[EpochContent] = ???

    override def getEpoch(epoch: Integer): Result[EpochContent] = ???

    override def getProtocolParameters(epoch: Integer): Result[ProtocolParams] = ???

    override def getProtocolParameters: Result[ProtocolParams] =
        val res = Result.success("dummy").asInstanceOf[Result[ProtocolParams]]
        res.withValue(pp)
        res

class ScriptServiceMock extends ScriptService:
    override def getScriptDatum(datumHash: String): Result[ScriptDatum] = ???

    override def getScriptDatumCbor(datumHash: String): Result[ScriptDatumCbor] = ???

    override def getNativeScriptJson(scriptHash: String): Result[JsonNode] = ???

    override def getPlutusScriptCbor(scriptHash: String): Result[String] = ???
