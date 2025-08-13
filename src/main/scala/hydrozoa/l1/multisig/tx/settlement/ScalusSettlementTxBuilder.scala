package hydrozoa.l1.multisig.tx.settlement

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
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.Address
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.txbuilder.BuilderContext
import hydrozoa.infra.Piper
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.*
import scalus.ledger.api
import scalus.ledger.api.Timelock
import scalus.ledger.api.Timelock.Signature

import scala.collection

class ScalusSettlementTxBuilder(
    backendService: BackendService,
    reader: HeadStateReader,
    builderContext: BuilderContext
) extends SettlementTxBuilder {

    override def mkSettlementTxDraft(
        r: SettlementRecipe
    ): Either[String, SettlementTx] = {

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
        val resolvedUtxos: Map[TransactionInput, TransactionOutput] = {
            val resolved = utxoIds.map(id =>
                bloxToScalusUtxoQuery(backendService, id.toScalus) match {
                    case x @ Left(err) => {
                        val isDeposit = r.deposits.contains(id)
                        return Left(
                          s"${if isDeposit then "Deposit" else "Treasury"} UTxO not found on L1 ledger during settlement transaction. Bloxbean error: " ++ x.toString
                        )
                    }
                    case Right(output) => id.toScalus -> output
                }
            )
            resolved.toMap
        }

        //////////////////////////////////////////////////////////////////////////
        // Outputs

        /////////////////////////////////////////////////
        // Withdrawals (outputs go to peers)

        val withdrawalOutputs = r.utxosWithdrawn.utxoMap.values.map(_.toScalus)

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
        // TODO: Pass the hash of the protocol parameters in the datum
        val treasuryDatum = toData(
          mkMultisigTreasuryDatum(r.majorVersion, ByteString.empty).toData
        )

        //////////////
        // Value
        val withdrawnValue: Value = withdrawalOutputs.foldLeft(Value.zero)((s, w) => s + w.value)

        // The new treasury value should be the sum of all inputs minus withdrawals minus fee
        // -- the inputs will be the deposits and the old treasury utxo
        // FIXME: factor out this calculation
        // TODO: this might not work as expected, since it will produce
        //  lists like that: ada,token,...,ada,...
        val inputsValue: Value = resolvedUtxos.values.foldLeft(Value.zero)((b, to) => b + to.value)

        val treasuryValue: Value =
            try { inputsValue - withdrawnValue }
            catch {
                case e: IllegalArgumentException =>
                    return Left(
                      s"Malformed value equal to `inputsValue - withdrawnValue` encountered: inputsValue = ${inputsValue}, withdrawnValue = ${withdrawnValue}"
                    )
            }

        try {
            // Build transaction using TxBuilder
            val tx = builderContext
                .withUtxo(resolvedUtxos)
                .buildNewTx
                .withInputs(utxoIds.toSet.map(_.toScalus))
                .withScript(headNativeScript, 0)
                .|>(builder =>
                    withdrawalOutputs.foldLeft(builder)((b, utxo) =>
                        b.payToAddress(utxo.address, utxo.value)
                    )
                )
                .payToAddress(headAddress, treasuryValue, treasuryDatum)
                .doFinalize

            Right(Tx(Cbor.encode(tx).toByteArray))
        } catch {
            case e: Exception => Left(s"Failed to build settlement transaction: ${e.getMessage}")
        }
    }
}
