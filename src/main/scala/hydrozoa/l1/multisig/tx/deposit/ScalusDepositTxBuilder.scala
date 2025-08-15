package hydrozoa.l1.multisig.tx.deposit

import com.bloxbean.cardano.client.backend.api.BackendService
import hydrozoa.infra.transitionary.{bloxToScalusUtxoQuery, emptyTxBody, toScalus}
import hydrozoa.node.state.{HeadStateReader, multisigRegime}
import hydrozoa.{AddressL1, Tx, TxIx, TxL1}
import io.bullet.borer.Cbor
import scalus.builtin.Data.toData
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyPaymentPart, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline

class ScalusDepositTxBuilder(backendService: BackendService, reader: HeadStateReader)
    extends DepositTxBuilder {

    override def buildDepositTxDraft(recipe: DepositTxRecipe): Either[String, (TxL1, TxIx)] = {

        bloxToScalusUtxoQuery(backendService, recipe.deposit) match {
            case Left(err) => Left(s"Scalus DepositTxBuilder failed: ${err}")
            case Right(utxoFunding) =>
                Right({
                    // TODO: we set the fee to 1 ada, but this doesn't need to be
                    val feeCoin = Coin(1_000_000)
                    val depositValue: Value =
                        Value(coin = Coin(recipe.depositAmount.toLong))
                    val headAddress: AddressL1 =
                        (reader.multisigRegime(_.headAddress))

                    val depositOutput: TransactionOutput = TransactionOutput(
                      address = headAddress,
                      value = depositValue,
                      datumOption = Some(Inline(toData(recipe.datum)))
                    )

                    val changeOutput: TransactionOutput = TransactionOutput(
                      address = utxoFunding.address,
                      value =
                          try {
                              utxoFunding.value - Value(
                                coin = feeCoin
                              ) - depositValue
                          } catch {
                              case _: IllegalArgumentException =>
                                  return Left(
                                    s"Malformed value equal to `utxoFunding.value - Value(feeCoin) - depositValue` encountered: utxoFunding.value = ${utxoFunding.value}, depositValue = ${depositValue}, feeCoin = ${feeCoin}"
                                  )
                          },
                      datumOption = None
                    )

                    val requiredSigner: AddrKeyHash = {
                        utxoFunding.address match
                            case shelleyAddress: ShelleyAddress =>
                                shelleyAddress.payment match
                                    case ShelleyPaymentPart.Key(hash) => hash
                                    case _ => return Left("deposit not at a pubkey address")
                            case _ => return Left("Could not get key hash for required signer")
                    }

                    val txBody = emptyTxBody.copy(
                      inputs = Set(recipe.deposit),
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

                    (Tx(tx), TxIx(ix))
                })

        }
    }
}
