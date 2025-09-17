package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.multisig.ledger.DappLedger
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.{addDummyVKeys, removeDummyVKeys, setAuxData}
import io.bullet.borer.Cbor
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.txbuilder.*

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
        utxoFunding: TransactionOutput,
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

    def build(recipe: Recipe): Either[TxBalancingError, DepositTx] = {
        val depositValue: Value =
            Value(coin = Coin(recipe.depositAmount.toLong))

        val builder = {
            val b1 = recipe.context.buildNewTx
                // Deposit output
                .payToScript(
                  address = recipe.headAddress,
                  value = depositValue,
                  datum = toData(recipe.datum)
                )
                // Change output
                .payTo(address = recipe.utxoFunding.address, value = Value.zero)
                .selectInputs(SelectInputs.particular(Set(recipe.deposit)))
                .setAuxData(MD(MD.L1TxTypes.Deposit, recipe.headAddress))
                .addDummyVKeys(1)

            LowLevelTxBuilder
                .balanceFeeAndChange(
                  initial = b1.tx,
                  changeOutputIdx = 1,
                  protocolParams = recipe.context.protocolParams,
                  resolvedUtxo = recipe.context.utxo,
                  evaluator = recipe.context.evaluator
                )
                .map(tx => removeDummyVKeys(1, tx))
        }

        builder.map(tx =>
            DepositTx(
              depositProduced = DepositUtxo(
                l1Input = recipe.deposit,
                l1OutputAddress = recipe.headAddress,
                l1OutputDatum = recipe.datum,
                l1OutputValue = depositValue.coin,
                l1RefScript = None
              ),
              tx = tx
            )
        )
    }
}
