package hydrozoa.l1.rulebased.tx.resolution

import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.quicktx.ScriptTx
import hydrozoa.{Tx, TxL1, UtxoIdL1}
import scalus.cardano.ledger.{AddrKeyHash, Coin, ExUnits, Hash, KeepRaw, Redeemer, Redeemers, Sized, TaggedSet, Transaction, TransactionBody, TransactionOutput, TransactionWitnessSet, Value, given_Ordering_TransactionInput}
import hydrozoa.infra.transitionary.*
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.{DisputeRedeemer, VoteDatum}
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.VoteStatus.{NoVote, Vote}
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.{ResolvedDatum, TreasuryDatum, TreasuryRedeemer}
import io.bullet.borer.Cbor
import scalus.builtin.Data.{fromData, toData}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.RedeemerTag.Spend
import scalus.cardano.ledger.ScriptDataHashGenerator.computeScriptDataHash
import scalus.cardano.ledger.TransactionOutput.Babbage

import scala.util.boundary
import scala.util.boundary.break

// FIXME: We set the fee to 1 ADA for now. This should be revised
val feeCoin = Coin(1000000)
// FIXME: Redeemers need to know how many exunit the script is allocated. This is a random amount
val redeemerEUAllocation = ExUnits(memory = 999_999_999, steps = 999_999_999)

class ScalusResolutionTxBuilder(
    backendService: BackendService,
    mbTreasuryScriptRefUtxoId: Option[UtxoIdL1],
    mbDisputeScriptRefUtxoId: Option[UtxoIdL1]
) extends ResolutionTxBuilder {
    override def buildResolutionTx(r: ResolutionTxRecipe): Either[String, TxL1] = {
        def fixedPoint (finalBody : TransactionBody, initial: TransactionBody) : TransactionBody = boundary{
            (mbTreasuryScriptRefUtxoId, mbTreasuryScriptRefUtxoId) match
            case (Some(treasuryScriptRefUtxoId), Some(disputeScriptRefUtxoId)) => {
                /////////////
                // Vote UTxO
              
      

                val voteRedeemer = Redeemer(
                    tag = Spend,
                    // We use the final body from the recursive fixed point here, because inputs come in sorted order
                    index = finalBody.inputs.toSeq.indexOf(r.talliedVote),
                    data = DisputeRedeemer.Resolve.toData,
                    // FIXME: How should this be set properly?
                    exUnits = redeemerEUAllocation
                )
                ////////////
                // Treasury UTxO
                val treasuryTO: TransactionOutput.Babbage =
                    bloxToScalusUtxoQuery(backendService, r.treasuryUtxoId) match {
                        case Right(to) =>
                            to match
                                case b: Babbage => b
                                case _ => throw RuntimeException("Treasury UTxO not a babbage UTxO")
                        case Left(err) => throw RuntimeException("resolving the treasury UTxO failed: " ++ err)
                    } 

                // FIXME: May lead to match error at runtime
                val TreasuryDatum.Unresolved(treasuryDatum) = treasuryTO.datumOption match {
                    case Some(Inline(d)) => fromData[TreasuryDatum](d)
                    case _ => throw RuntimeException("Treasury datum not an inline datum")
                }

                val outputTreasuryDatum = TreasuryDatum
                    .Resolved(
                        ResolvedDatum(
                            treasuryDatum.headMp,
                            vote.utxosActive,
                            (treasuryDatum.versionMajor, vote.versionMinor),
                            treasuryDatum.params
                        )
                    )
                val outputTreasuryValue: Value = voteTO.value + treasuryTO.value

                val treasuryOutput: TransactionOutput = Babbage(
                    address = treasuryTO.address,
                    value = outputTreasuryValue,
                    datumOption = Some(Inline(toData(outputTreasuryDatum))),
                    scriptRef = None
                )

                val treasuryRedeemer = Redeemer(
                    tag = Spend,
                    // We use the final body of the recursive fixed point because the inputs come in sorted order
                    index = finalBody.inputs.toSeq.indexOf(r.treasuryUtxoId),
                    data = TreasuryRedeemer.Resolve.toData,
                    exUnits = redeemerEUAllocation
                )

                ///////////
                // Assemble the Tx
                initialTxBody.copy(
                        inputs = Set(r.talliedVote.toScalus, r.treasuryUtxoId.toScalus),
                        outputs = IndexedSeq(treasuryOutput).map(Sized(_)),
                        // TODO: we set the fee to 1 ada, but this doesn't need to be
                        fee = feeCoin,
                        scriptDataHash = Some(
                            Hash(
                                computeScriptDataHash(
                                    era = r.era,
                                    redeemers =
                                        Some(KeepRaw(Redeemers.from(Seq(voteRedeemer, treasuryRedeemer)))),
                                    datums = KeepRaw(
                                        TaggedSet(
                                            Set(
                                                outputTreasuryDatum.toData,
                                                treasuryDatum.toData,
                                                voteDatum.toData
                                            )
                                                .map(d => KeepRaw(d))
                                        )
                                    ),
                                    costModels = r.costModels
                                )
                            )
                        ),
                        referenceInputs =
                            Set(treasuryScriptRefUtxoId.toScalus, disputeScriptRefUtxoId.toScalus),
                        // Do we need a mint? mint = ???,
                        requiredSigners = Set(AddrKeyHash.fromHex(r.nodeAccount.enterpriseAddress())),
                        // FIXME: This is copied from the BB builder -- it should actually be LEQ the proper deadline
                        ttl = Some(1024)
                    )}
                // FIXME: Why do we take an option if we immediately fail on None?
                case _ => throw RuntimeException("Ref scripts are not set")
                    
            }
        val initialTxBody: TransactionBody = emptyTxBody
        val txBody : TransactionBody = {
            lazy val result: TransactionBody = fixedPoint(result, initialTxBody)
            result
        }
        val scalusTransaction: Transaction = Transaction(
            body = KeepRaw(txBody),
            witnessSet = TransactionWitnessSet(???),
            isValid = true,
            auxiliaryData = None
        )




                Right(Tx(Cbor.encode(scalusTransaction).toByteArray))

            }

            
    }
