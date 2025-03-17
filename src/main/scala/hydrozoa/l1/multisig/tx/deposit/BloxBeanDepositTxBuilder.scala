package hydrozoa.l1.multisig.tx.deposit

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.Amount.ada
import com.bloxbean.cardano.client.api.model.{Amount, Utxo}
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.quicktx.Tx
import com.bloxbean.cardano.client.transaction.spec.Transaction
import hydrozoa.infra.{mkBuilder, toEither}
import hydrozoa.l1.multisig.state.given_ToData_DepositDatum
import hydrozoa.l1.multisig.tx.MultisigTxs.DepositTx
import hydrozoa.node.server.HeadStateReader
import hydrozoa.{AppCtx, TxAny, TxIx, TxL1}
import scalus.bloxbean.*
import scalus.builtin.Data.toData

import scala.jdk.CollectionConverters.*

class BloxBeanDepositTxBuilder(
    ctx: AppCtx,
    headStateReader: HeadStateReader
) extends DepositTxBuilder {

    private val backendService = ctx.backendService
    private val builder = mkBuilder[Tx](ctx)

    override def buildDepositTxDraft(r: DepositTxRecipe): Either[String, (DepositTx, TxIx)] =

        val Right(fundUtxo) = backendService.getUtxoService
            .getTxOutput(r.utxo._1.hash, r.utxo._2.ix.intValue)
            .toEither

        println(fundUtxo)

        val headAddressBech32 = headStateReader.headBechAddress

        // TODO: valueToAmountList(fundUtxo.toValue) OR we should ask for a value (might be easier)
        val amountList: List[Amount] = List(ada(100))
        val datum: PlutusData = Interop.toPlutusData(r.datum.toData)
        val depositorAddress = fundUtxo.getAddress

        val tx = Tx()
            .collectFrom(List(fundUtxo).asJava)
            .payToContract(headAddressBech32.bech32, amountList.asJava, datum)
            .from(depositorAddress)

        val ret: Transaction = builder
            .apply(tx)
            .withRequiredSigners(Address(depositorAddress))
            .feePayer(depositorAddress)
            .build()

        // Deposit output
        val index = ret.getBody.getOutputs.asScala
            .indexWhere(output => output.getAddress == headAddressBech32.bech32)

        Right(DepositTx(TxL1(ret.serialize)), TxIx(index))
}
