package hydrozoa.l1.rulebased.tx.deinit

import co.nstant.in.cbor.model.Array
import com.bloxbean.cardano.client.api.model.{Amount, Utxo as BBUtxo}
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.common.cbor.CborSerializationUtil
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.quicktx.ScriptTx
import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.infra.{encodeHex, mkBuilder, numberOfSignatories, toEither}
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.{TreasuryDatum, TreasuryRedeemer}
import hydrozoa.{TxL1, UtxoId, UtxoIdL1}
import scalus.bloxbean.*
import scalus.builtin.Data.{fromData, toData}
import scalus.builtin.FromData

import scala.jdk.CollectionConverters.*

class BloxBeanDeinitTxBuilder(
    backendService: BackendService,
    mbTreasuryScriptRefUtxoId: Option[UtxoIdL1]
) extends DeinitTxBuilder {

    private val builder = mkBuilder[ScriptTx](backendService, false)

    override def buildDeinitTxDraft(r: DeinitTxRecipe): Either[String, TxL1] =
        mbTreasuryScriptRefUtxoId match
            case Some(treasuryScriptRefUtxoId) =>
                def getUtxoWithDatum[T](using FromData[T])(utxoId: UtxoIdL1): (BBUtxo, T) =
                    val Right(utxo) = backendService.getUtxoService
                        .getTxOutput(utxoId.transactionId.toHex, utxoId.index)
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

                val treasuryValue = treasuryInput.toValue
                val treasuryCoins = treasuryValue.getCoin

                val headTokensToBurn = treasuryValue.getMultiAssets.asScala
                    .find(_.getPolicyId == encodeHex(IArray.from(r.headMintingPolicy.bytes)))
                    .get
                    .negate

                val treasuryRedeemer = Interop.toPlutusData(TreasuryRedeemer.Deinit.toData)

                val scriptArray =
                    CborSerializationUtil
                        .deserialize(r.headNativeScript.script.toCbor)
                        .asInstanceOf[Array]
                val headNativeScript = NativeScript.deserialize(scriptArray)

                // native multisig + one required signer to spend seed utxo
                val signatories = numberOfSignatories(headNativeScript) + 1

                //
                val txPartial = ScriptTx()
                    .collectFrom(treasuryInput, treasuryRedeemer)
                    .payToAddress(r.initializerAddress.toBech32.get, Amount.lovelace(treasuryCoins))
                    // Due to BB's peculiarities, we can't use .mintAsset from ScriptTx
                    // please see .preBalanceTx and .postBalanceTx down below.
                    // .mintAssets(headNativeScript, ???)
                    .readFrom(
                      treasuryScriptRefUtxoId.transactionId.toHex,
                      treasuryScriptRefUtxoId.index
                    )

                val nodeAddress = r.nodeAccount.enterpriseAddress()
                val txSigner = SignerProviders.signerFrom(r.nodeAccount)

                val tx: Transaction = builder
                    .apply(txPartial)
                    // .withRequiredSigners(Address(nodeAddress))
                    .collateralPayer(nodeAddress)
                    .feePayer(nodeAddress)
                    .withSigner(txSigner)
                    .additionalSignersCount(signatories)
                    // .preBalanceTx should be called only once
                    .preBalanceTx((_, t) =>
                        t.getWitnessSet.getNativeScripts.add(headNativeScript)
                        t.getBody.getMint.add(headTokensToBurn)
                    )
                    .postBalanceTx((_, t) =>
                        val outputs = t.getBody.getOutputs
                        // These are burned effectively
                        outputs.getLast.getValue.setMultiAssets(List.empty.asJava)
                    )
                    .buildAndSign()

                Right(TxL1(tx.serialize))

            case _ => Left("Ref scripts are not set")
}
