package hydrozoa.l1.multisig.tx.deposit

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.Amount.{lovelace}
import com.bloxbean.cardano.client.api.model.{Amount, Utxo}
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.quicktx.Tx
import com.bloxbean.cardano.client.transaction.spec.Transaction
import hydrozoa.infra.{mkBuilder, toEither}
import hydrozoa.l1.multisig.state.given_ToData_DepositDatum
import hydrozoa.node.state.{HeadStateReader, multisigRegime}
import hydrozoa.{TxIx, TxL1}
import scalus.bloxbean.*
import scalus.builtin.Data.toData

import java.math.BigInteger
import scala.jdk.CollectionConverters.*

class BloxBeanDepositTxBuilder(
    backendService: BackendService,
    reader: HeadStateReader
) extends DepositTxBuilder {

    private val builder = mkBuilder[Tx](backendService)

    override def buildDepositTxDraft(r: DepositTxRecipe): Either[String, (TxL1, TxIx)] =

        val Right(utxoFunding) = backendService.getUtxoService
            .getTxOutput(r.deposit._1.hash, r.deposit._2.ix.intValue)
            .toEither

        val headAddressBech32 = reader.multisigRegime(_.headBechAddress)
        val amountList: List[Amount] = List(lovelace(BigInteger.valueOf(r.depositAmount.longValue)))
        val datum: PlutusData = Interop.toPlutusData(r.datum.toData)
        val depositorAddress = utxoFunding.getAddress

        val txPartial = Tx()
            .collectFrom(List(utxoFunding).asJava)
            .payToContract(headAddressBech32.bech32, amountList.asJava, datum)
            .from(depositorAddress)

        val depositTx: Transaction = builder
            .apply(txPartial)
            .withRequiredSigners(Address(depositorAddress))
            .feePayer(depositorAddress)
            .build()

        // Find the deposit output index
        val index = depositTx.getBody.getOutputs.asScala
            .indexWhere(output => output.getAddress == headAddressBech32.bech32)

        assert(index >= 0, s"Deposit output was not found in the tx.")

        Right(TxL1(depositTx.serialize), TxIx(index.toChar))
}
