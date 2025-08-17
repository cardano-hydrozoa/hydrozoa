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

import scala.util.{Failure, Success, Try}

class ScalusDepositTxBuilder(backendService: BackendService, reader: HeadStateReader)
    extends DepositTxBuilder {

    override def buildDepositTxDraft(recipe: DepositTxRecipe): Either[String, (TxL1, TxIx)] = {
        for
            utxoFunding <- bloxToScalusUtxoQuery(backendService, recipe.deposit)

            // TODO: we set the fee to 1 ada, but this doesn't need to be
            feeCoin = Coin(1_000_000)
            depositValue: Value =
                Value(coin = Coin(recipe.depositAmount.toLong))
            headAddress: AddressL1 =
                (reader.multisigRegime(_.headAddress))

            depositOutput: TransactionOutput = TransactionOutput(
              address = headAddress,
              value = depositValue,
              datumOption = Some(Inline(toData(recipe.datum)))
            )

            changeOutputValue <- Try(
              utxoFunding.value - Value(
                coin = feeCoin
              ) - depositValue
            ) match {
                case Success(s) => Right(s)
                case Failure(e) =>
                    Left(
                      s"Malformed value equal to `utxoFunding.value - Value(feeCoin) - depositValue` encountered: utxoFunding.value = ${utxoFunding.value}, depositValue = ${depositValue}, feeCoin = ${feeCoin}"
                    )
            }

            changeOutput: TransactionOutput =
                TransactionOutput(
                  address = utxoFunding.address,
                  value = changeOutputValue,
                  datumOption = None
                )

            requiredSigner: AddrKeyHash <- {
                utxoFunding.address match
                    case shelleyAddress: ShelleyAddress =>
                        shelleyAddress.payment match
                            case ShelleyPaymentPart.Key(hash) => Right(hash)
                            case _ => Left("deposit not at a pubkey address")
                    case _ => Left("Could not get key hash for required signer")
            }

            txBody = emptyTxBody.copy(
              inputs = Set(recipe.deposit),
              outputs = IndexedSeq(depositOutput, changeOutput).map(Sized(_)),
              fee = feeCoin,
              requiredSigners = Set(requiredSigner)
            )

            tx: Transaction = Transaction(
              body = KeepRaw(txBody),
              witnessSet = TransactionWitnessSet.empty,
              isValid = true,
              auxiliaryData = None
            )

            // Find the deposit output index
            ix = tx.body.value.outputs.indexWhere(output => output.value.address == headAddress)

            _ = assert(ix >= 0, s"Deposit output was not found in the tx.")
        yield (Tx(tx), TxIx(ix))
    }
}
