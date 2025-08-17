package hydrozoa.l1.multisig.tx.settlement

import cats.implicits.*
import com.bloxbean.cardano.client.backend.api.BackendService
import hydrozoa.infra.transitionary.{bloxToScalusUtxoQuery, emptyTxBody}
import hydrozoa.l1.multisig.state.mkMultisigTreasuryDatum
import hydrozoa.l1.multisig.tx.{MultisigTx, SettlementTx}
import hydrozoa.node.state.{HeadStateReader, multisigRegime}
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.Script.Native

import scala.collection
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

class ScalusSettlementTxBuilder(
    backendService: BackendService,
    reader: HeadStateReader
) extends SettlementTxBuilder {

    override def mkSettlementTxDraft(
        r: SettlementRecipe
    ): Either[String, SettlementTx] = {
        val feeCoin = Coin(10000000)
        val utxoIds =
            r.deposits.toBuffer.append(reader.openPhaseReader(_.lastKnownTreasuryUtxoId))

        for

            //////////////////////////////////////////////////////////////////////////
            // Inputs (Absorbed deposits + old treasury Utxo)

            // We use a buffer here since despite the fact inputs are a set,
            // the order matters - different orders produce different txIds.

            // We can't use reader.multisigRegime(_.treasuryUtxoId) since `MultisigHeadStateL1`
            // holds information as L1 provider sees it.

            // So instead we have to use the last known settlement transaction with `lastKnownTreasuryUtxoId`.

            // This is a resolved list of all the inputs. It is impure; may fail if the lookup fails
            resolvedUtxoInputs: Seq[TransactionOutput] <-
                utxoIds
                    .map(id =>
                        bloxToScalusUtxoQuery(backendService, id) match {
                            case x @ Left(err) => {
                                val isDeposit = r.deposits.contains(id)

                                Left(
                                  s"${if isDeposit then "Deposit" else "Treasury"} UTxO not found on L1 ledger during settlement transaction. Bloxbean error: " ++ x.toString
                                )
                            }

                            case Right(output) => Right(output)
                        }
                    )
                    // N.B.: will return the first error only. We could switch to a validation type instead.
                    .toSeq
                    .traverse(identity)

            //////////////////////////////////////////////////////////////////////////
            // Outputs

            /////////////////////////////////////////////////
            // Withdrawals (outputs go to peers)

            withdrawals: IndexedSeq[Sized[TransactionOutput]] =
                r.utxosWithdrawn.values.toIndexedSeq.map(Sized(_))

            /////////////////////////////////////////////////
            // Treasury Output (with funds increased from deposits, decreased from withdrawals)

            ////////////////////////////
            // First set up our data

            //////////////
            // Address
            // CBOR encoded hydrozoa native script
            // TODO: Turn this into a helper function or revise the types; its duplicated in the refund tx builder
            headNativeScript: Native = reader.multisigRegime(_.headNativeScript)

            // TODO: factor this out or change types. It is shared with the deposit Tx builder
            headAddress: ShelleyAddress = reader.multisigRegime(_.headAddress)

            //////////////
            // Datum
            // TODO: Pass the hash of the protocol parameters in the datum
            treasuryDatum = toData(
              mkMultisigTreasuryDatum(r.majorVersion, ByteString.empty).toData
            )

            //////////////
            // Value
            withdrawnValue: Value = withdrawals.foldLeft(Value.zero)((s, w) => s + (w.value.value))

            // The new treasury value should be the sum of all inputs minus withdrawals minus fee
            // -- the inputs will be the deposits and the old treasury utxo
            // FIXME: factor out this calculation
            // TODO: this might not work as expected, since it will produce
            //  lists like that: ada,token,...,ada,...
            inputsValue: Value =
                resolvedUtxoInputs.foldLeft(Value.zero)((b, to) => b + to.value)

            treasuryValue: Value <- Try(
              inputsValue - withdrawnValue - Value(coin = feeCoin)
            ) match {
                case Success(s) => Right(s)
                case Failure(e: IllegalArgumentException) =>
                    Left(
                      s"Malformed value equal to `inputsValue - withdrawnValue - Value(coin = feeCoin)` encountered: inputsValue = ${inputsValue}, withdrawnValue = ${withdrawnValue}, feeCoin = ${feeCoin}"
                    )
                case Failure(otherE) =>
                    Left(s"mkSettlementTxDraft: unable to make treasury value. ${otherE}")
            }

            ////////////////////////////
            // Then construct the output

            treasuryOutput: Sized[TransactionOutput] = Sized(
              TransactionOutput(
                address = headAddress,
                value = treasuryValue,
                datumOption = Some(Inline(treasuryDatum))
              )
            )

            ///////////////////////////
            // Finally construct the body
            txBody =
                emptyTxBody.copy(
                  inputs = utxoIds.toSet.map(_.untagged),
                  outputs = withdrawals.appended(treasuryOutput),
                  // TODO: we set the fee to 1 ada, but this doesn't need to be
                  fee = feeCoin
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
        yield (MultisigTx(tx))

    }
}
