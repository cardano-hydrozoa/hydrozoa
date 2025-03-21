package hydrozoa.l1.multisig.tx.finalization

import com.bloxbean.cardano.client.api.util.AssetUtil
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.quicktx.Tx
import com.bloxbean.cardano.client.transaction.spec.Asset
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import hydrozoa.infra.{force, mkBuilder, toBloxBeanTransactionOutput}
import hydrozoa.l1.multisig.tx.{FinalizationTx, MultisigTx}
import hydrozoa.l2.ledger.state.unwrapTxOut
import hydrozoa.node.state.{HeadStateReader, multisigRegime}
import hydrozoa.{AppCtx, TxL1}

import java.math.BigInteger
import scala.jdk.CollectionConverters.*
import scala.language.postfixOps

class BloxBeanFinalizationTxBuilder(
    backendService: BackendService,
    reader: HeadStateReader
) extends FinalizationTxBuilder {

    private lazy val builder = mkBuilder[Tx](backendService)

    override def buildFinalizationTxDraft(
        r: FinalizationRecipe
    ): Either[String, FinalizationTx] =

        val treasuryUtxoId = reader.multisigRegime(_.currentTreasuryRef)
        val treasuryUtxo = backendService.getUtxoService
            .getTxOutput(treasuryUtxoId._1.hash, treasuryUtxoId._2.ix.intValue)
            .force

        val headNativeScript =
            NativeScript.deserializeScriptRef(reader.multisigRegime(_.headNativeScript).bytes)

        // Beacon token to burn
        val beaconTokenName = reader.multisigRegime(_.beaconTokenName)
        val beaconTokenAsset = AssetUtil.getUnit(headNativeScript.getPolicyId, beaconTokenName)
        val beaconTokenToBurn = Asset.builder
            .name(beaconTokenName)
            .value(BigInteger.valueOf(-1))
            .build

        val outputsToWithdraw =
            r.utxosWithdrawn.map(w => toBloxBeanTransactionOutput(w._2))

        val coinsWithdrawn =
            outputsToWithdraw.foldLeft(BigInteger.ZERO)((s, w) => s.add(w.getValue.getCoin))

        // TODO: do this arithmetic outside the builder
        // Treasury utxo value minus the beacon token
        val treasuryResidualValue = treasuryUtxo.getAmount.asScala.toList
            .filterNot(_.getUnit == beaconTokenAsset)

        // Subtract withdrawn lovelace
        treasuryResidualValue.foreach(a =>
            if a.getUnit.equals("lovelace") then
                a.setQuantity((a.getQuantity.subtract(coinsWithdrawn)))
        )
        //

        // Addresses
        val headAddressBech32 = reader.multisigRegime(_.headBechAddress).bech32
        val seedAddress = reader.multisigRegime(_.seedAddress).bech32

        val txPartial = Tx()
            .collectFrom(List(treasuryUtxo).asJava)
            .payToAddress(seedAddress, treasuryResidualValue.asJava)
            .mintAssets(headNativeScript, beaconTokenToBurn)
            .withChangeAddress(seedAddress)
            .from(headAddressBech32)

        val finalizationTx = builder
            .apply(txPartial)
            // NB: .preBalanceTx should be called only once
            .preBalanceTx((_, t) => t.getBody.getOutputs.addAll(outputsToWithdraw.asJava))
            .feePayer(seedAddress)
            // TODOk: magic numbers
            .additionalSignersCount(3)
            .build

        Right(MultisigTx(TxL1(finalizationTx.serialize)))
}
