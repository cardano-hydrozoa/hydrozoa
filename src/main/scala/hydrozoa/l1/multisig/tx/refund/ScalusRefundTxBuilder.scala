package hydrozoa.l1.multisig.tx.refund

import com.bloxbean.cardano.client.backend.api.BackendService
import hydrozoa.NativeScript
import hydrozoa.infra.transitionary.{emptyTxBody, toScalusNativeScript, v1AddressToLedger}
import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.{AddressBech, Tx}
import hydrozoa.l1.multisig.tx.PostDatedRefundTx
import hydrozoa.node.state.{HeadStateReader, multisigRegime}
import io.bullet.borer.Cbor
import scalus.builtin.ByteString
import scalus.builtin.Data.{fromData, toData}
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.address.Address.Shelley
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AssetName, Coin, KeepRaw, Sized, Transaction, TransactionInput, TransactionOutput, TransactionWitnessSet, VKeyWitness, Value}
import scalus.ledger.api
import scalus.ledger.api.Timelock
import scalus.ledger.api.Timelock.Signature

class ScalusRefundTxBuilder(
    backendService: BackendService,
    reader: HeadStateReader
) extends RefundTxBuilder {
    override def mkPostDatedRefundTxDraft(
        r: PostDatedRefundRecipe
    ): Either[String, PostDatedRefundTx] = {
        // N.B.: Fee is currently paid from the deposit itself
        val feeCoin = Coin(1000000)

        val depositOutput = r.depositTx.body.value.outputs(r.txIx.ix).value match {
            case to: TransactionOutput.Babbage => to
            case _                             => return Left("deposit output not a babbage output")
        }

        val depositDatum: DepositDatum = depositOutput.datumOption match {
            case None => return Left("deposit datum missing")
            case Some(datumO) =>
                datumO match
                    case Inline(d) => fromData[DepositDatum](d)
                    case _         => return Left("deposit datum not inline")
        }

        val refundOutput: TransactionOutput =
            TransactionOutput(
              address = Shelley(v1AddressToLedger(depositDatum.refundAddress, r.network)),
              value = depositOutput.value,
              datumOption = depositDatum.refundDatum.map(bs => Inline(toData(bs))).asScala
            )

        // TODO: temporary workaround - add 60 slots to the tip
        val validitySlot: Option[Long] = Some(
          (backendService.getBlockService.getLatestBlock.getValue.getSlot + 60)
        )

        // CBOR encoded hydrozoa native script
        // TODO: Turn this into a helper function or revise the types; its duplicated in the settlement tx builder
        val headNativeScript: Native =
              reader.multisigRegime(_.headNativeScript).toScalusNativeScript

        // TODO: factor out. Duplicated in Settlement Transaction
        val requiredSigners = headNativeScript.script match {
            case api.Timelock.AllOf(scripts) =>
                scripts.map {
                    case s: Signature => s.keyHash
                    // FIXME (Peter, 2025-07-11):
                    //  Warns that non-local returns are no longer supported and to
                    // use boundary/boundary break-in instead -- but I don't know what this
                    // is yet
                    case _ => return Left("Malformed native script: not a multisig")
                }.toSet
            case _ => return Left("Malformed native script: top level is not AllOf")
        }

        val txBody =
            emptyTxBody.copy(
              inputs = Set(TransactionInput(transactionId = r.depositTx.id, index = r.txIx.ix)),
              outputs = IndexedSeq(refundOutput).map(Sized(_)),
              // TODO: we set the fee to 1 ada, but this doesn't need to be
              fee = feeCoin,
              validityStartSlot = validitySlot,
              requiredSigners = requiredSigners
            )

        val txWitSet: TransactionWitnessSet =
            TransactionWitnessSet(
              nativeScripts = Set(headNativeScript)
            )
        val tx: Transaction = Transaction(
          body = KeepRaw(txBody),
          witnessSet = txWitSet,
          isValid = true,
          auxiliaryData = None
        )

        Right(Tx(Cbor.encode(tx).toByteArray))
    }

}
