package hydrozoa.l1.multisig.tx.initialization

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.Amount.{ada, asset}
import com.bloxbean.cardano.client.api.model.ProtocolParams
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, Tx}
import com.bloxbean.cardano.client.transaction.spec.Asset
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import hydrozoa.infra.toEither
import hydrozoa.l1.multisig.state.{given_ToData_MultisigTreasuryDatum, mkInitMultisigTreasuryDatum}
import hydrozoa.{AppCtx, L1Tx}
import scalus.bloxbean.*
import scalus.builtin.ByteString
import scalus.builtin.Data.toData

import java.math.BigInteger
import scala.jdk.CollectionConverters.*

class BloxBeanInitTxBuilder(ctx: AppCtx) extends InitTxBuilder {

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

    override def mkInitDraft(r: InitTxRecipe): Either[String, L1Tx] =
        for
            // TODO: Should be passed as an arg, but cannot be serialized easily.
            seedUtxo <- backendService.getUtxoService
                .getTxOutput(r.txId.hash, r.txIx.ix.intValue)
                .toEither

            beaconToken = Asset.builder
                .name(r.beaconTokenName)
                .value(BigInteger.valueOf(1))
                .build

            script = NativeScript.deserializeScriptRef(r.headNativeScript.bytes)

            treasuryValue = List(
              ada(r.amount),
              asset(script.getPolicyId, beaconToken.getName, BigInteger.valueOf(1))
            )

            treasuryDatum = Interop.toPlutusData(
              mkInitMultisigTreasuryDatum(ByteString.empty).toData
            )

            tx = Tx()
                .mintAssets(script, beaconToken)
                .collectFrom(List(seedUtxo).asJava)
                .payToContract(r.headAddressBech32, treasuryValue.asJava, treasuryDatum)
                .from(seedUtxo.getAddress)

            ret = quickTxBuilder
                .compose(tx)
                .withTxEvaluator(evaluator)
                .withRequiredSigners(Address(seedUtxo.getAddress))
                .additionalSignersCount(4) // TODO: magic number
                .build()
        yield L1Tx(ret.serialize())
}
