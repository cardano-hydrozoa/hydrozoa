package hydrozoa.l1.multisig.tx.refund

import com.bloxbean.cardano.client.api.model.Amount.lovelace
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.quicktx.Tx
import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import com.bloxbean.cardano.client.transaction.util.TransactionUtil.getTxHash
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.infra.{addressToBloxbean, mkBuilder, txOutputToUtxo}
import hydrozoa.l1.multisig.state.{DepositDatum, given_FromData_DepositDatum}
import hydrozoa.l1.multisig.tx.MultisigTxs.PostDatedRefundTx
import hydrozoa.node.server.HeadStateReader
import hydrozoa.{AppCtx, TxAny, TxL1}
import scalus.bloxbean.*
import scalus.builtin.Data.{fromCbor, fromData}
import scalus.prelude.Maybe.{Just, Nothing}

import scala.jdk.CollectionConverters.*
import scala.language.postfixOps

class BloxBeanRefundTxBuilder(
    ctx: AppCtx,
    headStateReader: HeadStateReader
) extends RefundTxBuilder {

    private val backendService = ctx.backendService
    private val builder = mkBuilder[Tx](ctx)

    override def mkPostDatedRefundTxDraft(
        r: PostDatedRefundRecipe
    ): Either[String, PostDatedRefundTx] =

        val txBytes = r.depositTx.toTxL1.bytes
        val tb = Transaction.deserialize(txBytes)
        val txHash = getTxHash(txBytes)
        val txIxInt = r.txIx.ix.intValue()
        val depositOutput = tb.getBody.getOutputs.get(txIxInt) // TODO: may throw
        val depositUtxo = txOutputToUtxo(txHash, txIxInt, depositOutput)

        val datum: DepositDatum = fromData[DepositDatum](
          Interop.toScalusData(
            PlutusData.deserialize(HexUtil.decodeHexString(depositUtxo.getInlineDatum))
          )
        )

        val headAddressBech32 = headStateReader.headBechAddress

        val refundAddress = addressToBloxbean(ctx.network, datum.refundAddress)

        // TODO: Not the best place
        // TODO: can be checked afterwards see https://github.com/cardano-hydrozoa/hydrozoa/issues/62
        // Deposit is locked at the head's script (maybe not necessary in fact)
        if (headAddressBech32.bech32 != depositUtxo.getAddress)
            return Left("Deposit utxo should be locked at the head's address.")

        // TODO: move to Node as well, not a concern of this function
        // TODO: this fails with Yaci - it doesn't expose GET /api/v1/genesis
        // val Right(genesis) = ctx.backendService.getNetworkInfoService.getNetworkInfo.toEither
        // val slotLength = genesis.getSlotLength
        // val slotZero = genesis.getSystemStart
        // val beginSlot = ...

        // TODO: temporary workaround - add 60 slots to the tip
        val lastBlockSlot = ctx.backendService.getBlockService.getLatestBlock.getValue.getSlot
        val beginSlot = lastBlockSlot + 60

        val tx = Tx()
            .collectFrom(List(depositUtxo).asJava)

        datum.refundDatum match
            case Nothing =>
                tx.payToAddress(refundAddress.toBech32, lovelace(depositOutput.getValue.getCoin))
            case Just(refundDatum) =>
                tx.payToContract(
                  refundAddress.toBech32,
                  lovelace(depositOutput.getValue.getCoin),
                  Interop.toPlutusData(fromCbor(refundDatum))
                )

        tx.from(headAddressBech32.bech32)

        val headNativeScript = headStateReader.headNativeScript
        val nativeScript = NativeScript.deserializeScriptRef(headNativeScript.bytes)

        val ret = builder
            .apply(tx)
            .validFrom(beginSlot)
            .preBalanceTx((_, t) => t.getWitnessSet.getNativeScripts.add(nativeScript))
            // TODO: magic numbers
            .additionalSignersCount(3)
            .feePayer(refundAddress.toBech32)
            .build

        Right(PostDatedRefundTx.apply(TxL1(ret.serialize)))
}
