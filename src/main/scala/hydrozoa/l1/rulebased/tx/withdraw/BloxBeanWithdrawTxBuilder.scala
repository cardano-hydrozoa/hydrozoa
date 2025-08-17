package hydrozoa.l1.rulebased.tx.withdraw

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.quicktx.ScriptTx
import com.bloxbean.cardano.client.transaction.spec.Transaction
import hydrozoa.*
import hydrozoa.infra.{getUtxoWithDatum, mkBuilder, toBloxBeanTransactionOutput, toTxOutRefV3}
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.{
    TreasuryDatum,
    TreasuryRedeemer,
    WithdrawRedeemer
}
import scalus.bloxbean.*
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.prelude.asScalus

import java.math.BigInteger
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

class BloxBeanWithdrawTxBuilder(
    backendService: BackendService,
    mbTreasuryScriptRefUtxoId: Option[UtxoIdL1]
) extends WithdrawTxBuilder {

    private val builder = mkBuilder[ScriptTx](backendService, false)

    override def buildWithdrawTx(r: WithdrawTxRecipe): Either[String, TxL1] =
        mbTreasuryScriptRefUtxoId match
            case Some(treasuryScriptRefUtxoId) =>
                for
                    treasuryInputAndDatum <-
                        getUtxoWithDatum[TreasuryDatum](r.resolvedTreasuryUtxoId, backendService)
                    treasuryInput = treasuryInputAndDatum._1
                    treasuryInputDatum <- treasuryInputAndDatum._2 match {
                        case TreasuryDatum.Resolved(tid) => Right(tid)
                        case _ => Left("buildWithdrawTx: treasury datum not resolved")
                    }

                    proofBS = ByteString.fromHex(r.proof)
                    outputDatum = Interop.toPlutusData(
                      TreasuryDatum
                          .Resolved(treasuryInputDatum.copy(utxosActive = proofBS))
                          .toData
                    )

                    withdrawals =
                        r.withdrawals
                            .map((_, utxo) => toBloxBeanTransactionOutput(Output[L2](utxo)))
                            .toSeq

                    withdrawnCoins =
                        withdrawals.foldLeft(BigInteger.ZERO)((s, w) => s.add(w.getValue.getCoin))

                    // FIXME: factor out this calculation
                    treasuryValue: mutable.Buffer[Amount] = treasuryInput.getAmount.asScala

                    // Subtract withdrawn lovelace
                    _ = treasuryValue.foreach(a =>
                        if a.getUnit.equals("lovelace") then
                            a.setQuantity((a.getQuantity.subtract(withdrawnCoins)))
                    )

                    withdrawRedeemer = WithdrawRedeemer(
                      r.withdrawals.keys.map(_.toTxOutRefV3).toList.asScalus,
                      proofBS
                    )

                    treasuryRedeemer =
                        Interop.toPlutusData(TreasuryRedeemer.Withdraw(withdrawRedeemer).toData)

                    //
                    txPartial = ScriptTx()
                        .collectFrom(treasuryInput, treasuryRedeemer)
                        .payToContract(treasuryInput.getAddress, treasuryValue.asJava, outputDatum)
                        .readFrom(
                          treasuryScriptRefUtxoId.transactionId.toHex,
                          treasuryScriptRefUtxoId.index
                        )

                    nodeAddress = r.nodeAccount.enterpriseAddress()
                    txSigner = SignerProviders.signerFrom(r.nodeAccount)

                    tx: Transaction = builder
                        .apply(txPartial)
                        // TODO: this should be LEQ than what an unresolved treasury datum contains
                        // see MajorBlockConfirmationActor.scala:210
                        .validTo(1024)
                        .withRequiredSigners(Address(nodeAddress))
                        .collateralPayer(nodeAddress)
                        .feePayer(nodeAddress)
                        .withSigner(txSigner)
                        // .preBalanceTx should be called only once
                        .preBalanceTx((_, t) =>
                            val outputs = t.getBody.getOutputs
                            outputs.addAll(withdrawals.asJava) : Unit
                        )
                        .buildAndSign()
                yield (TxL1(tx.serialize))

            case _ => Left("Ref scripts are not set")
}
