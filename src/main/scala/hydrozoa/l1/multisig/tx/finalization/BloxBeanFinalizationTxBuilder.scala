package hydrozoa.l1.multisig.tx.finalization

import com.bloxbean.cardano.client.api.model.{Amount, ProtocolParams}
import com.bloxbean.cardano.client.api.util.AssetUtil
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.coinselection.impl.{
    DefaultUtxoSelectionStrategyImpl,
    ExcludeUtxoSelectionStrategy
}
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, Tx}
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import com.bloxbean.cardano.client.transaction.spec.{Asset, TransactionInput}
import hydrozoa.infra.force
import hydrozoa.l1.multisig.tx.MultisigTxs.FinalizationTx
import hydrozoa.node.server.HeadStateReader
import hydrozoa.{AppCtx, L1Tx}
import scalus.bloxbean.*

import java.math.BigInteger
import scala.jdk.CollectionConverters.*
import scala.language.postfixOps

class BloxBeanFinalizationTxBuilder(
    ctx: AppCtx,
    headStateReader: HeadStateReader
) extends FinalizationTxBuilder {

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

    override def mkFinalization(
        r: FinalizationRecipe
    ): Either[String, FinalizationTx] =

        val treasury = headStateReader.currentTreasuryRef
        val treasuryUtxo = backendService.getUtxoService
            .getTxOutput(treasury._1.hash, treasury._2.ix.intValue)
            .force

        // Native script
        val Some(headNativeScript) = headStateReader.headNativeScript
        val script = NativeScript.deserializeScriptRef(headNativeScript.bytes)

        // Beacon token to burn
        val Some(beaconTokenName) = headStateReader.beaconTokenName
        val beaconTokenToBurn = Asset.builder
            .name(beaconTokenName)
            .value(BigInteger.valueOf(-1))
            .build

        // The rest of treasury utxo -beacon token -5 ADA for fees
        val treasuryValue: List[Amount] =
            treasuryUtxo.getAmount.asScala.toList
//                .filterNot(
//                  _.getUnit() == AssetUtil.getUnit(script.getPolicyId, beaconTokenName)
//                )
//                .map(a =>
//                    if a.getUnit == "lovelace" then
//                        a.setQuantity(a.getQuantity.subtract(BigInteger(("10000000"))))
//                        a
//                    else a
//                )

        println(s"Treasury: $treasuryValue")

        // Addresses
        val Some(headAddressBech32) = headStateReader.headBechAddress
        val Some(seedAddress) = headStateReader.seedAddress

        val tx = Tx()
            .collectFrom(List(treasuryUtxo).asJava)
            .payToAddress(seedAddress.bech32, treasuryValue.asJava)
//            .mintAssets(script, beaconTokenToBurn)
            // .withChangeAddress(seedAddress.bech32)
            .from(headAddressBech32.bech32)

        println(s"Deposits to protect: $r.depositsToProtect")

        // TODO: This doesn't work for some reason
        val utxoSupplier = DefaultUtxoSupplier(backendService.getUtxoService)
        val protectDepositsStrategy = ExcludeUtxoSelectionStrategy(
          DefaultUtxoSelectionStrategyImpl(utxoSupplier),
          r.depositsToProtect
              .map(ad =>
                  TransactionInput
                      .builder()
                      .transactionId(ad.txId.hash)
                      .index(ad.txIx.ix.intValue)
                      .build()
              )
              .asJava
        )

        val ret = quickTxBuilder
            .compose(tx)
//            .withUtxoSelectionStrategy(protectDepositsStrategy)
            .withTxEvaluator(evaluator)
            // TODO: magic numbers
            .additionalSignersCount(3)
            .build

        Right(FinalizationTx.apply(L1Tx(ret.serialize())))
}
