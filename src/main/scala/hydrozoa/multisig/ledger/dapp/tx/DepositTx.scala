package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.emptyTxBody
import hydrozoa.multisig.ledger.DappLedger
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import io.bullet.borer.Cbor
import scalus.builtin.Data.toData
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage

import scala.util.{Failure, Success, Try}

// TODO: Make opaque. Only `parse` and `build` should create deposit Txs.
// TODO: List out exactly the invariants we expect.
final case class DepositTx(
    depositProduced: DepositUtxo,
    override val tx: Transaction
) extends Tx

object DepositTx {
    case class Recipe(
        deposit: TransactionInput, // the only UTXO with deposit funds in user's wallet
        depositAmount: BigInt,
        datum: DepositUtxo.Datum, // datum for deposit utxo
        headAddress: ShelleyAddress,
        utxoFunding: TransactionOutput
    )

    sealed trait ParseError extends Throwable
    private case class MetadataParseError(e: MD.ParseError) extends ParseError
    case object NoUtxoAtHeadAddress extends ParseError
    private case class DepositUtxoError(e: DepositUtxo.DepositUtxoConversionError)
        extends ParseError
    case class TxCborDeserializationFailed(e: Throwable) extends ParseError
    case class MultipleUtxosAtHeadAddress(numUtxos: Int) extends ParseError

    /** Parse a deposit transaction, ensuring that there is exactly one Babbage Utxo at the head
      * address (given in the transaction metadata) with an Inline datum that parses correctly.
      */
    def parse(txBytes: Tx.Serialized): Either[ParseError, DepositTx] = {
        given OriginalCborByteArray = OriginalCborByteArray(txBytes)
        Cbor.decode(txBytes).to[Transaction].valueTry match {
            case Success(tx) =>
                for {
                    // Pull head address from metadata
                    headAddress <- MD
                        .parseExpected(tx, MD.L1TxTypes.Deposit)
                        .left
                        .map(MetadataParseError.apply)
                    // Grab the single output at the head address, along with its index/
                    depositUtxoWithIndex <- tx.body.value.outputs.zipWithIndex
                        .filter(_._1.value.address == headAddress) match {
                        case x if x.size == 1 => Right(x.head)
                        case x if x.isEmpty   => Left(NoUtxoAtHeadAddress)
                        case x                => Left(MultipleUtxosAtHeadAddress(x.size))
                    }
                    // Check that the output is babbage, extract and parse its inline datum
                    dutxo <- DepositUtxo
                        .fromUtxo(
                          (
                            TransactionInput(tx.id, depositUtxoWithIndex._2),
                            depositUtxoWithIndex._1.value
                          )
                        )
                        .left
                        .map(DepositUtxoError(_))
                } yield DepositTx(
                  dutxo,
                  tx
                )
            case Failure(e) => Left(TxCborDeserializationFailed(e))
        }

    }

    def build(recipe: Recipe): Either[String, DepositTx] = {
        // TODO: we set the fee to 1 ada, but this doesn't need to be
        val feeCoin = Coin(1_000_000)
        val depositValue: Value =
            Value(coin = Coin(recipe.depositAmount.toLong))

        val depositOutput: Babbage = Babbage(
          address = recipe.headAddress,
          value = depositValue,
          datumOption = Some(Inline(toData(recipe.datum)))
        )
        for {

            changeOutputValue <- Try(
              recipe.utxoFunding.value - Value(
                coin = feeCoin
              ) - depositValue
            ) match {
                case Success(s) => Right(s)
                case Failure(e) =>
                    Left(
                      s"Malformed value equal to `utxoFunding.value - Value(feeCoin) - depositValue`" +
                          s" encountered: utxoFunding.value = ${recipe.utxoFunding.value}, " +
                          s"depositValue = $depositValue, feeCoin = $feeCoin"
                    )
            }

            changeOutput: TransactionOutput =
                TransactionOutput(
                  address = recipe.utxoFunding.address,
                  value = changeOutputValue,
                  datumOption = None
                )

            requiredSigner: AddrKeyHash <- {
                recipe.utxoFunding.address match
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
              auxiliaryData = Some(MD(MD.L1TxTypes.Deposit, recipe.headAddress))
            )

            // Find the deposit output index
            ix = tx.body.value.outputs.indexWhere(output =>
                output.value.address == recipe.headAddress
            )

            _ = assert(ix >= 0, s"Deposit output was not found in the tx.")
        } yield DepositTx(
          depositProduced = DepositUtxo(
            l1Input = recipe.deposit,
            l1OutputAddress = recipe.headAddress,
            l1OutputDatum = recipe.datum,
            l1OutputValue = depositValue.coin,
            l1RefScript = None
          ),
          tx = tx
        )
    }
}
