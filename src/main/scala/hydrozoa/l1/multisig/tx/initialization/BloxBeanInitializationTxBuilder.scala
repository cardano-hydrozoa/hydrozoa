package hydrozoa.l1.multisig.tx.initialization

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.Amount.{ada, asset, lovelace}
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.quicktx.Tx
import com.bloxbean.cardano.client.transaction.spec.Asset
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import hydrozoa.infra.{mkBuilder, toEither}
import hydrozoa.l1.multisig.state.{given_ToData_MultisigTreasuryDatum, mkInitMultisigTreasuryDatum}
import hydrozoa.l1.multisig.tx.{InitializationTx, MultisigTx}
import hydrozoa.{AddressBechL1, AppCtx, TxL1}
import scalus.bloxbean.*
import scalus.builtin.Data.toData

import java.math.BigInteger
import scala.jdk.CollectionConverters.*

class BloxBeanInitializationTxBuilder(backendService: BackendService) extends InitTxBuilder {

    private val builder = mkBuilder[Tx](backendService)

    /** @param recipe
      *   recipe
      * @return
      *   error or a tuple - tx + seed address
      */
    override def mkInitializationTxDraft(
        recipe: InitTxRecipe
    ): Either[String, (InitializationTx, AddressBechL1)] =
        for
            // TODO: Should be passed as an arg, but cannot be serialized easily.
            seedUtxo <- backendService.getUtxoService
                .getTxOutput(recipe.seedOutput.txId.hash, recipe.seedOutput.outputIx.ix.intValue)
                .toEither

            beaconToken = Asset.builder
                .name(recipe.beaconTokenName)
                .value(BigInteger.valueOf(1))
                .build

            script = NativeScript.deserializeScriptRef(recipe.headNativeScript.bytes)

            treasuryValue = List(
              lovelace(BigInteger.valueOf(recipe.coins)),
              asset(script.getPolicyId, beaconToken.getName, BigInteger.valueOf(1))
            )

            treasuryDatum = Interop.toPlutusData(
              mkInitMultisigTreasuryDatum.toData
            )

            txPartial = Tx()
                .mintAssets(script, beaconToken)
                .collectFrom(List(seedUtxo).asJava)
                .payToContract(recipe.headAddressBech32, treasuryValue.asJava, treasuryDatum)
                .from(seedUtxo.getAddress)

            initializationTx = builder
                .apply(txPartial)
                .withRequiredSigners(Address(seedUtxo.getAddress))
                // TODO: magic number
                .additionalSignersCount(4)
                .build()
        yield (MultisigTx(TxL1(initializationTx.serialize)), AddressBechL1(seedUtxo.getAddress))
}
