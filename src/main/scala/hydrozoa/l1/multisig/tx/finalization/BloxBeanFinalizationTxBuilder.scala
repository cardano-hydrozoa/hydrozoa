package hydrozoa.l1.multisig.tx.finalization

import com.bloxbean.cardano.client.api.util.AssetUtil
import com.bloxbean.cardano.client.quicktx.Tx
import com.bloxbean.cardano.client.transaction.spec.Asset
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import hydrozoa.infra.{force, mkBuilder, toBloxBeanTransactionOutput}
import hydrozoa.l1.multisig.tx.{FinalizationTx, MultisigTx}
import hydrozoa.l2.ledger.state.unwrapTxOut
import hydrozoa.node.server.HeadStateReader
import hydrozoa.{AppCtx, TxL1}

import java.math.BigInteger
import scala.jdk.CollectionConverters.*
import scala.language.postfixOps

class BloxBeanFinalizationTxBuilder(
    ctx: AppCtx,
    headStateReader: HeadStateReader
) extends FinalizationTxBuilder {

    private lazy val backendService = ctx.backendService
    private lazy val builder = mkBuilder[Tx](ctx)

    override def buildFinalizationTxDraft(
        r: FinalizationRecipe
    ): Either[String, FinalizationTx] =

        val treasury = headStateReader.currentTreasuryRef
        val treasuryUtxo = backendService.getUtxoService
            .getTxOutput(treasury._1.hash, treasury._2.ix.intValue)
            .force

        // Native script
        val headNativeScript = headStateReader.headNativeScript
        val script = NativeScript.deserializeScriptRef(headNativeScript.bytes)

        // Beacon token to burn
        val beaconTokenName = headStateReader.beaconTokenName
        val beaconTokenAsset = AssetUtil.getUnit(script.getPolicyId, beaconTokenName)
        val beaconTokenToBurn = Asset.builder
            .name(beaconTokenName)
            .value(BigInteger.valueOf(-1))
            .build

        val outputsWithdrawn =
            r.utxosWithdrawn.map(w => toBloxBeanTransactionOutput(unwrapTxOut(w._2)))

        val withdrawnAda =
            outputsWithdrawn.foldLeft(BigInteger.ZERO)((s, w) => s.add(w.getValue.getCoin))

        // Treasury utxo value minus the beacon token
        val treasuryValue = treasuryUtxo.getAmount.asScala.toList
            .filterNot(_.getUnit == beaconTokenAsset)

        // Subtract withdrawn lovelace
        treasuryValue.foreach(a =>
            if a.getUnit.equals("lovelace") then
                a.setQuantity((a.getQuantity.subtract(withdrawnAda)))
        )

        // Addresses
        val headAddressBech32 = headStateReader.headBechAddress
        val seedAddress = headStateReader.seedAddress

        val tx = Tx()
            .collectFrom(List(treasuryUtxo).asJava)
            .payToAddress(seedAddress.bech32, treasuryValue.asJava)
            .mintAssets(script, beaconTokenToBurn)
            .withChangeAddress(seedAddress.bech32)
            .from(headAddressBech32.bech32)

        val ret = builder
            .apply(tx)
            // Should be one
            .preBalanceTx((_, t) => t.getBody.getOutputs.addAll(outputsWithdrawn.asJava))
            .feePayer(seedAddress.bech32)
            // TODO: magic numbers
            .additionalSignersCount(3)
            .build

        Right(MultisigTx(TxL1(ret.serialize)))
}
