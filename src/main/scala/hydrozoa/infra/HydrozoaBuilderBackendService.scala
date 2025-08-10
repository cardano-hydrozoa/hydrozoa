package hydrozoa.infra

import com.bloxbean.cardano.client.api.common.OrderEnum
import com.bloxbean.cardano.client.api.helper.{FeeCalculationService, TransactionHelperService, UtxoTransactionBuilder}
import com.bloxbean.cardano.client.api.model.{Result, Utxo}
import com.bloxbean.cardano.client.backend.api.*
import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import hydrozoa.{Address, L1, TxL1}
import scalus.cardano.address.{ShelleyAddress, Address as SAddress}

import java.util
import scala.jdk.CollectionConverters.*

class HydrozoaBuilderBackendService(backendService: BackendService, multisigTx: TxL1)
    extends BackendService {

    override def getAccountService: AccountService = backendService.getAccountService
    override def getAddressService: AddressService = backendService.getAddressService
    override def getAssetService: AssetService = backendService.getAssetService
    override def getBlockService: BlockService = backendService.getBlockService
    override def getEpochService: EpochService = backendService.getEpochService
    override def getFeeCalculationService(
        transactionHelperService: TransactionHelperService
    ): FeeCalculationService = backendService.getFeeCalculationService(transactionHelperService)
    override def getFeeCalculationService: FeeCalculationService =
        backendService.getFeeCalculationService
    override def getMetadataService: MetadataService = backendService.getMetadataService
    override def getNetworkInfoService: NetworkInfoService = backendService.getNetworkInfoService
    override def getPoolService: PoolService = backendService.getPoolService
    override def getScriptService: ScriptService = backendService.getScriptService
    override def getTransactionHelperService: TransactionHelperService =
        backendService.getTransactionHelperService
    override def getTransactionService: TransactionService = backendService.getTransactionService
    override def getUtxoService: UtxoService = VirtualTreasuryUtxoService()
    override def getUtxoTransactionBuilder: UtxoTransactionBuilder =
        backendService.getUtxoTransactionBuilder

    final private class VirtualTreasuryUtxoService extends UtxoService {
        override def getUtxos(address: String, count: Int, page: Int): Result[util.List[Utxo]] = ???

        val txBytes = multisigTx.toCbor
        val txHash = TransactionUtil.getTxHash(txBytes)

        override def getUtxos(
            address: String,
            count: Int,
            page: Int,
            order: OrderEnum
        ): Result[util.List[Utxo]] =
            if page > 1 then ResultUtils.mkResultError
            else
                onlyOutputToAddress(multisigTx, Address[L1](SAddress.fromBech32(address).asInstanceOf[ShelleyAddress])) match {
                    case Left(_) => ResultUtils.mkResultError
                    case Right(treasuryOutputIx, _, multisigTreasuryDatum) =>
                        val tb = Transaction.deserialize(txBytes)
                        val treasuryOutput = tb.getBody.getOutputs.get(treasuryOutputIx)

                        // This is required only due to BB's peculiarities
                        val multisigTreasuryUtxo: Utxo =
                            txOutputToUtxo(txHash, treasuryOutputIx, treasuryOutput)

                        val result = Result.success("").asInstanceOf[Result[util.List[Utxo]]]
                        result.withValue(List(multisigTreasuryUtxo).asJava)
                        result
                }

        override def getUtxos(
            address: String,
            unit: String,
            count: Int,
            page: Int
        ): Result[util.List[Utxo]] = getUtxos(address, count, page, OrderEnum.asc)

        override def getUtxos(
            address: String,
            unit: String,
            count: Int,
            page: Int,
            order: OrderEnum
        ): Result[util.List[Utxo]] = getUtxos(address, count, page, order)

        override def getTxOutput(txHash: String, outputIndex: Int): Result[Utxo] =
            // if txHash == this.txHash && outputIndex == = then
            Result.error("ahh...").asInstanceOf[Result[Utxo]]
    }

}
