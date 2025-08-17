package hydrozoa.l1.rulebased.tx.deinit

import co.nstant.in.cbor.model.Array
import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.common.cbor.CborSerializationUtil
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.quicktx.ScriptTx
import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import hydrozoa.infra.{encodeHex, getUtxoWithDatum, mkBuilder, numberOfSignatories}
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.{TreasuryDatum, TreasuryRedeemer}
import hydrozoa.{TxL1, UtxoId, UtxoIdL1}
import scalus.bloxbean.*
import scalus.builtin.Data.toData

import scala.jdk.CollectionConverters.*

class BloxBeanDeinitTxBuilder(
    backendService: BackendService,
    mbTreasuryScriptRefUtxoId: Option[UtxoIdL1]
) extends DeinitTxBuilder {

    private val builder = mkBuilder[ScriptTx](backendService, false)

    override def buildDeinitTxDraft(r: DeinitTxRecipe): Either[String, TxL1] =
        mbTreasuryScriptRefUtxoId match
            case Some(treasuryScriptRefUtxoId) =>
                for
                    inputAndDatum <-
                        getUtxoWithDatum[TreasuryDatum](r.resolvedTreasuryUtxoId, backendService)

                    treasuryInput = inputAndDatum._1
                    treasuryInputDatum <- inputAndDatum._2 match {
                        case TreasuryDatum.Resolved(tid) => Right(tid)
                        case _ => Left("buildDeInitTxDraft: Treasury datum must be resolved")
                    }

                    treasuryValue = treasuryInput.toValue
                    treasuryCoins = treasuryValue.getCoin

                    headTokensToBurn = treasuryValue.getMultiAssets.asScala
                        .find(_.getPolicyId == encodeHex(IArray.from(r.headMintingPolicy.bytes)))
                        .get
                        .negate

                    treasuryRedeemer = Interop.toPlutusData(TreasuryRedeemer.Deinit.toData)

                    scriptArray =
                        CborSerializationUtil
                            .deserialize(r.headNativeScript.script.toCbor)
                            .asInstanceOf[Array]
                    headNativeScript = NativeScript.deserialize(scriptArray)

                    // native multisig + one required signer to spend seed utxo
                    signatories = numberOfSignatories(headNativeScript) + 1

                    txPartial = ScriptTx()
                        .collectFrom(treasuryInput, treasuryRedeemer)
                        .payToAddress(
                          r.initializerAddress.toBech32.get,
                          Amount.lovelace(treasuryCoins)
                        )
                        // Due to BB's peculiarities, we can't use .mintAsset from ScriptTx
                        // please see .preBalanceTx and .postBalanceTx down below.
                        // .mintAssets(headNativeScript, ???)
                        .readFrom(
                          treasuryScriptRefUtxoId.transactionId.toHex,
                          treasuryScriptRefUtxoId.index
                        )

                    nodeAddress = r.nodeAccount.enterpriseAddress()
                    txSigner = SignerProviders.signerFrom(r.nodeAccount)

                    tx: Transaction = builder
                        .apply(txPartial)
                        // .withRequiredSigners(Address(nodeAddress))
                        .collateralPayer(nodeAddress)
                        .feePayer(nodeAddress)
                        .withSigner(txSigner)
                        .additionalSignersCount(signatories)
                        // .preBalanceTx should be called only once
                        .preBalanceTx((_, t) =>
                            t.getWitnessSet.getNativeScripts.add(headNativeScript)
                            t.getBody.getMint.add(headTokensToBurn) : Unit
                        )
                        .postBalanceTx((_, t) =>
                            val outputs = t.getBody.getOutputs
                            // These are burned effectively
                            outputs.getLast.getValue.setMultiAssets(List.empty.asJava)
                        )
                        .buildAndSign()
                yield (TxL1(tx.serialize))

            case _ => Left("Ref scripts are not set")
}
