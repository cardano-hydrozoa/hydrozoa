package hydrozoa.l1.multisig.tx.deposit

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.Amount.{ada, asset}
import com.bloxbean.cardano.client.api.model.{Amount, ProtocolParams, Result}
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, Tx}
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import com.bloxbean.cardano.client.transaction.spec.{Asset, Transaction}
import hydrozoa.l1.multisig.state.{given_ToData_MultisigTreasuryDatum, mkInitMultisigTreasuryDatum}
import hydrozoa.l1.multisig.state.given_ToData_DepositDatum
import hydrozoa.node.server.HeadStateReader
import hydrozoa.{AppCtx, L1Tx, TxIx}
import scalus.bloxbean.*
import scalus.builtin.ByteString
import scalus.builtin.Data.toData

import java.math.BigInteger
import scala.jdk.CollectionConverters.*

extension [A](result: Result[A])
    def toEither: Either[String, A] =
        if result.isSuccessful then Right(result.getValue)
        else Left(result.getResponse)

// TODO factor out common parts into a separate component
class BloxBeanDepositTxBuilder(ctx: AppCtx, headStateReader: HeadStateReader)
    extends DepositTxBuilder {

    private val backendService = ctx.backendService

    lazy val protocolParams: ProtocolParams = {
        val result = backendService.getEpochService.getProtocolParameters
        if !result.isSuccessful then sys.error(result.getResponse)
        result.getValue
    }
    private lazy val quickTxBuilder = QuickTxBuilder(backendService)

    private lazy val utxoSupplier = new DefaultUtxoSupplier(backendService.getUtxoService)

    private lazy val evaluator = ScalusTransactionEvaluator(
      slotConfig = SlotConfig.Preprod,
      protocolParams = protocolParams,
      utxoSupplier = utxoSupplier,
      scriptSupplier = NoScriptSupplier(),
      mode = EvaluatorMode.EVALUATE_AND_COMPUTE_COST
    )

    override def mkDepositTx(r: DepositTxRecipe): Either[String, (L1Tx, TxIx)] =
        for
            fundUtxo <- backendService.getUtxoService
                .getTxOutput(r.utxo._1.hash, r.utxo._2.ix.intValue)
                .toEither

            headAddressBech32: String = ???
            amountList: List[Amount] = ??? // valueToAmountList(fundUtxo.toValue)
            datum: PlutusData = Interop.toPlutusData(r.datum.toData)

            tx = Tx()
                .collectFrom(List(fundUtxo).asJava)
                .payToContract(headAddressBech32, amountList.asJava, datum)
                .from(fundUtxo.getAddress)

            ret: Transaction = quickTxBuilder
                .compose(tx)
                .withTxEvaluator(evaluator)
                .withRequiredSigners(Address(fundUtxo.getAddress))
                .build()

            // FIXME return -1, though it can't be the case by construction
            index = ret.getBody.getOutputs.asScala
                .indexWhere(_.getAddress.equals(headAddressBech32))
        yield (L1Tx(ret.serialize()), TxIx(index))
}
