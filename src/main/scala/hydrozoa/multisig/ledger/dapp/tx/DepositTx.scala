package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import cats.syntax.all.*
import hydrozoa.multisig.ledger.DappLedger
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.tx.DepositTx.BuildError.{
    OtherScalusBalancingError,
    OtherScalusTransactionException
}
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.{addDummyVKeys, addOutputs, removeDummyVKeys, setAuxData}
import io.bullet.borer.Cbor
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.txbuilder.*

import scala.util.{Failure, Success}

// TODO: Make opaque. Only `parse` and `build` should create deposit Txs.
// TODO: List out exactly the invariants we expect.
final case class DepositTx(
    depositProduced: DepositUtxo,
    override val tx: Transaction
) extends Tx

object DepositTx {
    case class Recipe(
        depositAmount: Coin,
        datum: DepositUtxo.Datum,
        headAddress: ShelleyAddress,
        utxosFunding: NonEmptyList[(TransactionInput, TransactionOutput)],
        changeAddress: ShelleyAddress,
        context: BuilderContext
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

    enum BuildError:
        case OtherScalusBalancingError(e: TxBalancingError)
        case OtherScalusTransactionException(e: TransactionException)

    def build(recipe: Recipe): Either[BuildError, DepositTx] = {
        val depositValue: Value =
            Value(coin = recipe.depositAmount)

        val b1 = recipe.context.buildNewTx
            // Deposit output
            .payToScript(
              address = recipe.headAddress,
              value = depositValue,
              datum = toData(recipe.datum)
            )
            // Change output
            .addOutputs(
              List(
                Babbage(
                  address = recipe.changeAddress,
                  value = Value.zero,
                  datumOption = None,
                  scriptRef = None
                )
              )
            )
            .withInputs((recipe.utxosFunding.toList.toSet.map(_._1)))
            .setAuxData(MD(MD.L1TxTypes.Deposit, recipe.headAddress))
            .addDummyVKeys(1)

        for {
            balanced <- LowLevelTxBuilder
                .balanceFeeAndChange(
                  initial = b1.tx,
                  changeOutputIdx = 1,
                  protocolParams = recipe.context.protocolParams,
                  resolvedUtxo = recipe.context.utxo,
                  evaluator = recipe.context.evaluator
                )
                .map(tx => removeDummyVKeys(1, tx))
                .left
                .map(OtherScalusBalancingError(_))
            validated <- recipe.context
                .validate(balanced)
                .left
                .map(OtherScalusTransactionException(_))
        } yield DepositTx(
          depositProduced = DepositUtxo(
            l1Input = TransactionInput(validated.id, 0),
            l1OutputAddress = recipe.headAddress,
            l1OutputDatum = recipe.datum,
            l1OutputValue = depositValue.coin,
            l1RefScript = None
          ),
          tx = validated
        )

    }
}
