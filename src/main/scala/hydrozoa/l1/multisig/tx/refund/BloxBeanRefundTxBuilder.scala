package hydrozoa.l1.multisig.tx.refund

import com.bloxbean.cardano.client.api.model.Amount.lovelace
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.quicktx.Tx
import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import com.bloxbean.cardano.client.transaction.util.TransactionUtil.getTxHash
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.TxL1
import hydrozoa.infra.{
    addressToBloxbean,
    mkBuilder,
    numberOfSignatories,
    toBloxbean,
    txOutputToUtxo
}
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.state.{DepositDatum, given_FromData_DepositDatum}
import hydrozoa.l1.multisig.tx.{MultisigTx, PostDatedRefundTx, toL1Tx}
import hydrozoa.node.state.{HeadStateReader, multisigRegime}
import scalus.bloxbean.*
import scalus.builtin.Data.{fromCbor, fromData}
import scalus.prelude.Maybe.{Just, Nothing}

import scala.jdk.CollectionConverters.*
import scala.language.postfixOps

class BloxBeanRefundTxBuilder(
    cardanoL1: CardanoL1,
    backendService: BackendService,
    reader: HeadStateReader
) extends RefundTxBuilder {

    private val builder = mkBuilder[Tx](backendService)

    override def mkPostDatedRefundTxDraft(
        r: PostDatedRefundRecipe
    ): Either[String, PostDatedRefundTx] =

        val txBytes = r.depositTx.toL1Tx.bytes
        val tb = Transaction.deserialize(txBytes)
        val txHash = getTxHash(txBytes)
        val txIxInt = r.txIx.ix.toInt
        val depositOutput = tb.getBody.getOutputs.get(txIxInt) // TODO: may throw
        val depositUtxo = txOutputToUtxo(txHash, txIxInt, depositOutput)

        val datum: DepositDatum = fromData[DepositDatum](
          Interop.toScalusData(
            PlutusData.deserialize(HexUtil.decodeHexString(depositUtxo.getInlineDatum))
          )
        )

        val headAddressBech32 = reader.multisigRegime(_.headBechAddress).bech32

        val refundAddress = addressToBloxbean(cardanoL1.network.toBloxbean, datum.refundAddress)

        // TODO: Not the best place
        // TODO: can be checked afterwards see https://github.com/cardano-hydrozoa/hydrozoa/issues/62
        // Deposit is locked at the head's headNativeScript (maybe not necessary)
        if (headAddressBech32 != depositUtxo.getAddress)
            return Left("Deposit utxo should be locked at the head's address.")

        // TODO: move to Node as well, not a concern of this function
        // TODO: this fails with Yaci - it doesn't expose GET /api/v1/genesis
        // val Right(genesis) = ctx.backendService.getNetworkInfoService.getNetworkInfo.toEither
        // val slotLength = genesis.getSlotLength
        // val slotZero = genesis.getSystemStart
        // val beginSlot = ...

        // TODO: temporary workaround - add 60 slots to the tip
        val lastBlockSlot = backendService.getBlockService.getLatestBlock.getValue.getSlot
        val beginSlot = lastBlockSlot + 60

        val txPartial = Tx()
            .collectFrom(List(depositUtxo).asJava)

        datum.refundDatum match
            case Nothing =>
                txPartial.payToAddress(
                  refundAddress.toBech32,
                  lovelace(depositOutput.getValue.getCoin)
                )
            case Just(refundDatum) =>
                txPartial.payToContract(
                  refundAddress.toBech32,
                  lovelace(depositOutput.getValue.getCoin),
                  Interop.toPlutusData(fromCbor(refundDatum))
                )

        txPartial.from(headAddressBech32)

        val headNativeScript =
            NativeScript.deserializeScriptRef(reader.multisigRegime(_.headNativeScript).bytes)

        val postDatedRefundTx = builder
            .apply(txPartial)
            .validFrom(beginSlot)
            .preBalanceTx((_, t) => t.getWitnessSet.getNativeScripts.add(headNativeScript))
            .additionalSignersCount(numberOfSignatories(headNativeScript))
            .feePayer(refundAddress.toBech32)
            .build

        Right(MultisigTx(TxL1(postDatedRefundTx.serialize)))
}
