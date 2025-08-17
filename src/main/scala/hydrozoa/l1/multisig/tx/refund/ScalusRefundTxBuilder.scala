package hydrozoa.l1.multisig.tx.refund

import com.bloxbean.cardano.client.backend.api.BackendService
import hydrozoa.infra.Piper
import hydrozoa.infra.transitionary.{emptyTxBody, toScalusLedger}
import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.l1.multisig.tx.{MultisigTx, PostDatedRefundTx}
import hydrozoa.node.state.{HeadStateReader, multisigRegime}
import hydrozoa.{Address, Tx}
import scalus.builtin.Data.fromData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.Script.Native
import scalus.ledger.api
import scalus.ledger.api.Timelock
import scalus.ledger.api.Timelock.Signature

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

class ScalusRefundTxBuilder(
    backendService: BackendService,
    reader: HeadStateReader
) extends RefundTxBuilder {
    override def mkPostDatedRefundTxDraft(
        r: PostDatedRefundRecipe
    ): Either[String, PostDatedRefundTx] = {
        for
            depositOutput <- r.depositTx.body.value.outputs(r.txIx).value match {
                case to: TransactionOutput.Babbage => Right(to)
                case _                             => Left("deposit output not a babbage output")
            }
            // N.B.: Fee is currently paid from the deposit itself
            feeCoin = Coin(1000000)

            depositDatum: DepositDatum <- Try(
              depositOutput.datumOption.get.asInstanceOf[Inline].data |> fromData[DepositDatum]
            ) match {
                case Success(d) => Right(d)
                case Failure(e) =>
                    Left(
                      s"mkPostDatedRefundTxDraft: Could not deserialize inline deposit datum: ${e}"
                    )
            }

            refundOutput: TransactionOutput =
                TransactionOutput(
                  address = depositDatum.refundAddress.toScalusLedger,
                  value = depositOutput.value,
                  datumOption = depositDatum.refundDatum.asScala.map(Inline(_))
                )

            // TODO: temporary workaround - add 60 slots to the tip
            validitySlot: Option[Long] = Some(
              (backendService.getBlockService.getLatestBlock.getValue.getSlot + 60)
            )

            // CBOR encoded hydrozoa native script
            // TODO: Turn this into a helper function or revise the types; its duplicated in the settlement tx builder
            headNativeScript: Native =
                reader.multisigRegime(_.headNativeScript)

            // TODO: factor out. Duplicated in Settlement Transaction
            requiredSigners <- Try(
              headNativeScript.script
                  .asInstanceOf[api.Timelock.AllOf]
                  .scripts
                  .map(s => s.asInstanceOf[Signature].keyHash)
                  .toSet
            ) match {
                case Success(s) => Right(s)
                case Failure(e) =>
                    Left(s"mkPostDatedRefundTxDraft: encountered malformed headNativeScript. ${e}")
            }

            txBody =
                emptyTxBody.copy(
                  inputs = Set(TransactionInput(transactionId = r.depositTx.id, index = r.txIx)),
                  outputs = IndexedSeq(refundOutput).map(Sized(_)),
                  // TODO: we set the fee to 1 ada, but this doesn't need to be
                  fee = feeCoin,
                  validityStartSlot = validitySlot,
                  requiredSigners = requiredSigners
                )

            txWitSet: TransactionWitnessSet =
                TransactionWitnessSet(
                  nativeScripts = Set(headNativeScript)
                )
            tx: Transaction = Transaction(
              body = KeepRaw(txBody),
              witnessSet = txWitSet,
              isValid = true,
              auxiliaryData = None
            )
        yield (MultisigTx(Tx(tx)))
    }

}
