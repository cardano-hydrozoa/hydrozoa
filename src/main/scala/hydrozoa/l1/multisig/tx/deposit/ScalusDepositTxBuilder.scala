package hydrozoa.l1.multisig.tx.deposit

import com.bloxbean.cardano.client.backend.api.BackendService
import hydrozoa.infra.transitionary.{bloxToScalusUtxoQuery, emptyTxBody, toScalus}
import hydrozoa.node.state.{HeadStateReader, multisigRegime}
import hydrozoa.{Tx, TxIx, TxL1}
import io.bullet.borer.Cbor
import scalus.builtin.Data.toData
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyPaymentPart, StakePayload}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.*

class ScalusDepositTxBuilder(backendService: BackendService, reader: HeadStateReader)
    extends DepositTxBuilder {

    override def buildDepositTxDraft(recipe: DepositTxRecipe): Either[String, (TxL1, TxIx)] = {

        bloxToScalusUtxoQuery(backendService, recipe.deposit.toScalus) match {
            case Left(err) => Left(s"Scalus DepositTxBuilder failed: ${err}")
            case Right(utxoFunding) =>
                Right({
                    // TODO: we set the fee to 1 ada, but this doesn't need to be
                    val feeCoin = Coin(1_000_000)
                    val depositValue: Value =
                        Value(coin = Coin(recipe.depositAmount.toLong))
                    val headAddress: Address =
                        Address.fromBech32(reader.multisigRegime(_.headBechAddress).bech32)

                    val depositOutput: TransactionOutput = TransactionOutput(
                      address = headAddress,
                      value = depositValue,
                      datumOption = Some(Inline(toData(recipe.datum)))
                    )

                    val changeOutput: TransactionOutput = TransactionOutput(
                      address = utxoFunding.address,
                      value = utxoFunding.value - Value(
                        coin = feeCoin
                      ) - depositValue,
                      datumOption = None
                    )

                    val requiredSigner : AddrKeyHash  = { utxoFunding.address match
                            case shelleyAddress : ShelleyAddress =>
                                shelleyAddress.payment match
                                    case ShelleyPaymentPart.Key(hash) => hash
                                    case _ => return Left("deposit not at a pubkey address")
                            case _ => return Left("Could not get key hash for required signer")
                    }

                    val txBody = emptyTxBody.copy(
                      inputs = Set(recipe.deposit.toScalus),
                      outputs = IndexedSeq(depositOutput, changeOutput).map(Sized(_)),
                      fee = feeCoin,
                      requiredSigners = Set(utxoFunding.address.keyHash.get match {
                          case a: AddrKeyHash => a
                          case s: StakeKeyHash =>
                              return Left(
                                "buildDepositTxDraft: expecting an address key hash, got a stake key"
                              )

                      })
                    )

                    val tx: Transaction = Transaction(
                      body = KeepRaw(txBody),
                      witnessSet = TransactionWitnessSet.empty,
                      isValid = true,
                      auxiliaryData = None
                    )

                    // Find the deposit outout index
                    val ix = tx.body.value.outputs.indexWhere(output =>
                        output.value.address == headAddress
                    )

                    assert(ix >= 0, s"Deposit output was not found in the tx.")

                    (Tx(Cbor.encode(tx).toByteArray), TxIx(ix))
                })

        }
    }
}
