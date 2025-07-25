package hydrozoa.l1.rulebased.tx.withdraw

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.{Amount, Utxo as BBUtxo}
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.quicktx.ScriptTx
import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.infra.{mkBuilder, toBloxBeanTransactionOutput, toEither, toTxOutRefV3}
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.{
    TreasuryDatum,
    TreasuryRedeemer,
    WithdrawRedeemer
}
import hydrozoa.{TxL1, UtxoId, UtxoIdL1}
import scalus.bloxbean.*
import scalus.builtin.Data.{fromData, toData}
import scalus.builtin.{ByteString, FromData}
import scalus.prelude.List.asScalus

import java.math.BigInteger
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class BloxBeanWithdrawTxBuilder(
    backendService: BackendService,
    mbTreasuryScriptRefUtxoId: Option[UtxoIdL1]
) extends WithdrawTxBuilder {

    private val builder = mkBuilder[ScriptTx](backendService, false)

    override def buildWithdrawTx(r: WithdrawTxRecipe): Either[String, TxL1] =
        mbTreasuryScriptRefUtxoId match
            case Some(treasuryScriptRefUtxoId) =>
                def getUtxoWithDatum[T](using FromData[T])(utxoId: UtxoIdL1): (BBUtxo, T) =
                    val Right(utxo) = backendService.getUtxoService
                        .getTxOutput(utxoId.txId.hash, utxoId.outputIx.ix)
                        .toEither

                    val datum = fromData[T](
                      Interop.toScalusData(
                        PlutusData
                            .deserialize(HexUtil.decodeHexString(utxo.getInlineDatum))
                      )
                    )
                    (utxo, datum)

                val (treasuryInput: BBUtxo, TreasuryDatum.Resolved(treasuryInputDatum)) =
                    getUtxoWithDatum[TreasuryDatum](r.resolvedTreasuryUtxoId)

                val proofBS = ByteString.fromHex(r.proof)
                val outputDatum = Interop.toPlutusData(
                  TreasuryDatum
                      .Resolved(treasuryInputDatum.copy(utxosActive = proofBS))
                      .toData
                )

                val withdrawals =
                    r.withdrawals.utxoMap.map((_, utxo) => toBloxBeanTransactionOutput(utxo)).toSeq

                val withdrawnCoins =
                    withdrawals.foldLeft(BigInteger.ZERO)((s, w) => s.add(w.getValue.getCoin))

                // FIXME: factor out this calculation
                val treasuryValue: mutable.Buffer[Amount] = treasuryInput.getAmount.asScala

                // Subtract withdrawn lovelace
                treasuryValue.foreach(a =>
                    if a.getUnit.equals("lovelace") then
                        a.setQuantity((a.getQuantity.subtract(withdrawnCoins)))
                )

                val withdrawRedeemer = WithdrawRedeemer(
                  r.withdrawals.utxoMap.keys.map(_.toTxOutRefV3).toList.asScalus,
                  proofBS
                )

                val treasuryRedeemer =
                    Interop.toPlutusData(TreasuryRedeemer.Withdraw(withdrawRedeemer).toData)

                //
                val txPartial = ScriptTx()
                    .collectFrom(treasuryInput, treasuryRedeemer)
                    .payToContract(treasuryInput.getAddress, treasuryValue.asJava, outputDatum)
                    .readFrom(
                      treasuryScriptRefUtxoId.txId.hash,
                      treasuryScriptRefUtxoId.outputIx.ix
                    )

                val nodeAddress = r.nodeAccount.enterpriseAddress()
                val txSigner = SignerProviders.signerFrom(r.nodeAccount)

                val tx: Transaction = builder
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
                        outputs.addAll(withdrawals.asJava)
                    )
                    .buildAndSign()

                Right(TxL1(tx.serialize))

            case _ => Left("Ref scripts are not set")
}
