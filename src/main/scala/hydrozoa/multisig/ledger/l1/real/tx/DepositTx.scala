package hydrozoa.multisig.ledger.l1.real.tx

import hydrozoa.{AddressL1, L1, Output, TxIx, Utxo, UtxoIdL1, bloxToScalusUtxoQuery, emptyTxBody}
import hydrozoa.multisig.ledger.l1.real.LedgerL1.{DepositUtxo, Tx}
import hydrozoa.multisig.ledger.l1.real.state.DepositDatum
import hydrozoa.multisig.ledger.l1.real.{LedgerL1, txCborToScalus}
import scalus.cardano.ledger.UTxOState
import com.bloxbean.cardano.client.backend.api.BackendService
import scalus.builtin.Data.toData
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage

import scala.util.{Failure, Success, Try}

trait DepositTxBuilder {

    /** @param recipe
      * @return
      *   deposit tx draft + output index that points to the deposit utxo
      */
    def buildDepositTxDraft(recipe: DepositTxRecipe): Either[String, (LedgerL1.Tx, TxIx)]
}

case class DepositTxRecipe(
    deposit: UtxoIdL1, // the only UTXO with deposit funds in user's wallet
    depositAmount: BigInt,
    datum: DepositDatum, // datum for deposit utxo
    headAddress: AddressL1,
    headPolicyId: PolicyId // Note: not meaningfully used
)

class ScalusDepositTxBuilder(backendService: BackendService) extends DepositTxBuilder {

    override def buildDepositTxDraft(
        recipe: DepositTxRecipe
    ): Either[String, (LedgerL1.Tx.Deposit, TxIx)] = {
        for
            utxoFunding <- bloxToScalusUtxoQuery(backendService, recipe.deposit)

            // TODO: we set the fee to 1 ada, but this doesn't need to be
            feeCoin = Coin(1_000_000)
            depositValue: Value =
                Value(coin = Coin(recipe.depositAmount.toLong))

            depositOutput: Babbage = Babbage(
              address = recipe.headAddress,
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
            ix = tx.body.value.outputs.indexWhere(output =>
                output.value.address == recipe.headAddress
            )

            _ = assert(ix >= 0, s"Deposit output was not found in the tx.")
        yield (
          Tx.Deposit(
            depositProduced =
                DepositUtxo(Utxo(UtxoIdL1(TransactionInput(tx.id, ix)), Output[L1](depositOutput))),
            headAddress = recipe.headAddress,
            headPolicyId = recipe.headPolicyId,
            tx = tx
          ),
          TxIx(ix)
        )
    }
}
