package hydrozoa.l1.multisig.tx.initialization

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.Amount.{asset, lovelace}
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.quicktx.Tx
import com.bloxbean.cardano.client.transaction.spec.Asset
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import hydrozoa.infra.{mkBuilder, toEither}
import hydrozoa.l1.multisig.state.{given_ToData_MultisigTreasuryDatum, mkInitMultisigTreasuryDatum}
import hydrozoa.l1.multisig.tx.{InitializationTx, MultisigTx}
import hydrozoa.{AddressBechL1, TxL1}
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
        // TODO: Should be passed as an arg, but cannot be serialized easily.
        val Right(seedUtxo) = backendService.getUtxoService
            .getTxOutput(recipe.seedOutput.txId.hash, recipe.seedOutput.outputIx.ix.intValue)
            .toEither

        val beaconToken = Asset.builder
            .name(recipe.beaconTokenName)
            .value(BigInteger.valueOf(1))
            .build

        val script = NativeScript.deserializeScriptRef(recipe.headNativeScript.bytes)

        val treasuryValue = List(
          lovelace(BigInteger.valueOf(recipe.coins)),
          asset(script.getPolicyId, beaconToken.getName, BigInteger.valueOf(1))
        )

        val treasuryDatum = Interop.toPlutusData(mkInitMultisigTreasuryDatum.toData)

        val seederAddress = seedUtxo.getAddress

        val txPartial = Tx()
            .mintAssets(script, beaconToken)
            .collectFrom(List(seedUtxo).asJava)
            .payToContract(recipe.headAddressBech32, treasuryValue.asJava, treasuryDatum)
            .from(seederAddress)

        val initializationTx = builder
            .apply(txPartial)
            .feePayer(seederAddress)
            .withRequiredSigners(Address(seederAddress))
            // TODO: magic number
            .additionalSignersCount(4)
            .build()

        Right(MultisigTx(TxL1(initializationTx.serialize)), AddressBechL1(seederAddress))
}
