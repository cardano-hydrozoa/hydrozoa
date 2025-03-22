package hydrozoa.l1.multisig.tx.deposit

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.Amount.ada
import com.bloxbean.cardano.client.api.model.{Amount, Utxo}
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.quicktx.Tx
import com.bloxbean.cardano.client.transaction.spec.Transaction
import hydrozoa.infra.{mkBuilder, toEither}
import hydrozoa.l1.multisig.state.given_ToData_DepositDatum
import hydrozoa.l1.multisig.tx.{DepositTx, MultisigTx}
import hydrozoa.node.state.{HeadStateReader, multisigRegime}
import hydrozoa.{AppCtx, TxIx, TxL1}
import scalus.bloxbean.*
import scalus.builtin.Data.toData

import scala.jdk.CollectionConverters.*

class BloxBeanDepositTxBuilder(
    backendService: BackendService,
    reader: HeadStateReader
) extends DepositTxBuilder {

    private val builder = mkBuilder[Tx](backendService)

    override def buildDepositTxDraft(r: DepositTxRecipe): Either[String, (DepositTx, TxIx)] =

        val Right(fundUtxo) = backendService.getUtxoService
            .getTxOutput(r.deposit._1.hash, r.deposit._2.ix.intValue)
            .toEither

        val headAddressBech32 = reader.multisigRegime(_.headBechAddress)

        // TODO: valueToAmountList(fundUtxo.toValue) OR we should ask for a value (might be easier)
        val amountList: List[Amount] = List(ada(100))
        val datum: PlutusData = Interop.toPlutusData(r.datum.toData)
        val depositorAddress = fundUtxo.getAddress

        val txPartial = Tx()
            .collectFrom(List(fundUtxo).asJava)
            .payToContract(headAddressBech32.bech32, amountList.asJava, datum)
            .from(depositorAddress)

        val depositTx: Transaction = builder
            .apply(txPartial)
            .withRequiredSigners(Address(depositorAddress))
            .feePayer(depositorAddress)
            .build()

        // Deposit output
        val index = depositTx.getBody.getOutputs.asScala
            .indexWhere(output => output.getAddress == headAddressBech32.bech32)

        Right(MultisigTx(TxL1(depositTx.serialize)), TxIx(index))
}
