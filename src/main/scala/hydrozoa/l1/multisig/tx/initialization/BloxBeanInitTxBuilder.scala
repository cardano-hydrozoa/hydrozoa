package hydrozoa.l1.multisig.tx.initialization

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.Amount.{ada, asset}
import com.bloxbean.cardano.client.quicktx.Tx
import com.bloxbean.cardano.client.transaction.spec.Asset
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import hydrozoa.infra.{mkBuilder, toEither}
import hydrozoa.l1.multisig.state.{given_ToData_MultisigTreasuryDatum, mkInitMultisigTreasuryDatum}
import hydrozoa.{AddressBechL1, AppCtx, L1Tx}
import scalus.bloxbean.*
import scalus.builtin.ByteString
import scalus.builtin.Data.toData

import java.math.BigInteger
import scala.jdk.CollectionConverters.*

class BloxBeanInitTxBuilder(ctx: AppCtx) extends InitTxBuilder {

    private val backendService = ctx.backendService
    private val builder = mkBuilder[Tx](ctx)

    /** @param r
      *   recipe
      * @return
      *   error or a tuple - tx + seed address
      */
    override def mkInitializationTxDraft(r: InitTxRecipe): Either[String, (L1Tx, AddressBechL1)] =
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
              mkInitMultisigTreasuryDatum(0, ByteString.empty).toData
            )

            tx = Tx()
                .mintAssets(script, beaconToken)
                .collectFrom(List(seedUtxo).asJava)
                .payToContract(r.headAddressBech32, treasuryValue.asJava, treasuryDatum)
                .from(seedUtxo.getAddress)

            ret = builder
                .apply(tx)
                .withRequiredSigners(Address(seedUtxo.getAddress))
                // TODO: magic number
                .additionalSignersCount(4)
                .build()
        yield (L1Tx(ret.serialize()), AddressBechL1(seedUtxo.getAddress))
}
