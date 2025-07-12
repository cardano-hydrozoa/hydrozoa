package hydrozoa.l1.multisig.tx.settlement

import scalus.builtin.ByteString
import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.backend.api.BackendService
import hydrozoa.Tx
import hydrozoa.infra.force
import hydrozoa.infra.transitionary.{bloxToScalusUtxoQuery, emptyTxBody, toScalus}
import hydrozoa.l1.multisig.state.mkMultisigTreasuryDatum
import hydrozoa.l1.multisig.tx.SettlementTx
import hydrozoa.node.state.{HeadStateReader, multisigRegime}
import io.bullet.borer.Cbor
import scalus.bloxbean.Interop.toPlutusData
import scalus.cardano.address.Address
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.{
    Coin,
    KeepRaw,
    Sized,
    Transaction,
    TransactionInput,
    TransactionOutput,
    TransactionWitnessSet,
    Value
}
import scalus.builtin.Data.toData
import scalus.cardano.ledger.DatumOption.Inline
import scalus.ledger.api
import scalus.ledger.api.Timelock
import scalus.ledger.api.Timelock.Signature

import scala.collection

class ScalusSettlementTxBuilder(
    backendService: BackendService,
    reader: HeadStateReader
) extends SettlementTxBuilder {

    override def mkSettlementTxDraft(
        r: SettlementRecipe
    ): Either[String, SettlementTx] = {
        val feeCoin = Coin(10000000)

        //////////////////////////////////////////////////////////////////////////
        // Inputs (Absorbed deposits + old treasury Utxo)

        // We use a buffer here since despite the fact inputs are a set,
        // the order matters - different orders produce different txIds.

        // We can't use reader.multisigRegime(_.treasuryUtxoId) since `MultisigHeadStateL1`
        // holds information as L1 provider sees it.

        // So instead we have to use the last known settlement transaction with `lastKnownTreasuryUtxoId`.

        val utxoIds =
            r.deposits.toBuffer.append(reader.openPhaseReader(_.lastKnownTreasuryUtxoId))

        // This is a resolved list of all the inputs. It is impure; may fail if the lookup fails
        val resolvedUtxoInputs: Seq[TransactionOutput] =
            utxoIds
                .map(deposit => bloxToScalusUtxoQuery(backendService, deposit))
                .map {
                    case x @ Left(err) =>
                        return Left(
                          "UTxO input (i.e., deposit or treasury UTxO) not found during settlement transaction. Bloxbean error: " ++ x.toString
                        )
                    case Right(output) => output
                }
                .toSeq

        //////////////////////////////////////////////////////////////////////////
        // Outputs

        /////////////////////////////////////////////////
        // Withdrawals (outputs go to peers)

        val withdrawals: IndexedSeq[Sized[TransactionOutput]] =
            r.utxosWithdrawn.utxoMap.map(w => w._2.toScalus).toIndexedSeq.map(Sized(_))

        /////////////////////////////////////////////////
        // Treasury Output (with funds increased from deposits, decreased from withdrawals)

        ////////////////////////////
        // First set up our data

        //////////////
        // Address
        // CBOR encoded hydrozoa native script
        // TODO: Turn this into a helper function or revise the types; its duplicated in the refund tx builder
        val headNativeScript: Native =
            Cbor.decode(reader.multisigRegime(_.headNativeScript).bytes).to[Native].value

        // TODO: factor this out or change types. It is shared with the deposit Tx builder
        val headAddress: Address =
            Address.fromBech32(reader.multisigRegime(_.headBechAddress).bech32)

        //////////////
        // Datum
        val treasuryDatum = toData(
          mkMultisigTreasuryDatum(r.majorVersion, ByteString.empty).toData
        )

        //////////////
        // Value
        val withdrawnValue: Value = withdrawals.foldLeft(Value.zero)((s, w) => s + (w.value.value))

        // The new treasury value should be the sum of all inputs minus withdrawals minus fee
        // -- the inputs will be the deposits and the old treasury utxo
        // FIXME: factor out this calculation
        // TODO: this might not work as expected, since it will produce
        //  lists like that: ada,token,...,ada,...
        val inputsValue: Value =
            resolvedUtxoInputs.foldLeft(Value.zero)((b, to) => b + to.value)

        val treasuryValue: Value =
            inputsValue - withdrawnValue - Value(coin = feeCoin, multiAsset = Map.empty)

        ////////////////////////////
        // Then construct the output

        val treasuryOutput: Sized[TransactionOutput] = Sized(
          TransactionOutput(
            address = headAddress,
            value = treasuryValue,
            datumOption = Some(Inline(treasuryDatum))
          )
        )

        ///////////////////////////
        // Finally construct the body
        val txBody =
            emptyTxBody.copy(
              inputs = utxoIds.toSet.map(_.toScalus),
              outputs = withdrawals.appended(treasuryOutput),
              // TODO: we set the fee to 1 ada, but this doesn't need to be
              fee = feeCoin
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
