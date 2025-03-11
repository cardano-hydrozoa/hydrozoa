package hydrozoa.l1.multisig.tx.settlement

import com.bloxbean.cardano.client.api.model.{Amount, Utxo}
import com.bloxbean.cardano.client.quicktx.Tx
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import hydrozoa.infra.{force, mkBuilder}
import hydrozoa.l1.multisig.state.{given_ToData_MultisigTreasuryDatum, mkInitMultisigTreasuryDatum}
import hydrozoa.l1.multisig.tx.MultisigTxs.SettlementTx
import hydrozoa.node.server.HeadStateReader
import hydrozoa.{AppCtx, L1Tx}
import scalus.bloxbean.*
import scalus.builtin.ByteString
import scalus.builtin.Data.toData

import scala.jdk.CollectionConverters.*
import scala.language.postfixOps

class BloxBeanSettlementTxBuilder(
    ctx: AppCtx,
    headStateReader: HeadStateReader
) extends SettlementTxBuilder {

    private val backendService = ctx.backendService
    private val builder = mkBuilder[Tx](ctx)

    override def mkSettlementTxDraft(
        r: SettlementRecipe
    ): Either[String, SettlementTx] =

        val refs = r.deposits.map(d => (d.txId, d.txIx)) + headStateReader.currentTreasuryRef

        val utxos: Set[Utxo] =
            refs.map(r =>
                backendService.getUtxoService.getTxOutput(r._1.hash, r._2.ix.intValue).force
            )

        val treasuryValue: List[Amount] = utxos.toList.flatMap(u => u.getAmount.asScala)

        val headAddressBech32 = headStateReader.headBechAddress

        val treasuryDatum = Interop.toPlutusData(
          mkInitMultisigTreasuryDatum(r.majorVersion, ByteString.empty).toData
        )

        val tx = Tx()
            .collectFrom(utxos.asJava)
            .payToContract(
              headAddressBech32.bech32,
              treasuryValue.asJava,
              treasuryDatum
            )
            .from(headAddressBech32.bech32)

        val headNativeScript = headStateReader.headNativeScript
        val nativeScript = NativeScript.deserializeScriptRef(headNativeScript.bytes)

        val ret = builder
            .apply(tx)
            .preBalanceTx((_, t) => t.getWitnessSet.getNativeScripts.add(nativeScript))
            // TODO: magic numbers
            .additionalSignersCount(3)
            .build

        Right(SettlementTx.apply(L1Tx(ret.serialize())))
}
