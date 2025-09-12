package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.emptyTxBody
import hydrozoa.multisig.ledger.dapp.DappLedger
import DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import io.bullet.borer.Cbor
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Script.Native

import scala.collection.immutable.SortedMap
import scala.language.implicitConversions
import scala.util.{Failure, Success}

final case class FinalizationTx(
    private val treasurySpent: TreasuryUtxo,
    override val tx: Transaction
) extends Tx

object FinalizationTx {
    case class Recipe(
        majorVersion: Int,
        utxosWithdrawn: Set[(TransactionInput, TransactionOutput)],
        headNativeScript: HeadMultisigScript,
        headTokenName: AssetName,
        treasuryUtxo: TreasuryUtxo,
        changeAddress: ShelleyAddress
    )

    sealed trait BuildError extends Throwable
    case object IllegalChangeValue extends BuildError

    def build(recipe: Recipe): Either[BuildError, FinalizationTx] = {
        val feeCoin: Coin = Coin(1000000)
        val beaconTokenBurn = Mint(
          MultiAsset(
            SortedMap.from(
              Seq(
                (
                  recipe.headNativeScript.policyId,
                  SortedMap.from(Seq((recipe.headTokenName, -1.toLong)))
                )
              )
            )
          )
        )

        val valueWithdrawn: Value =
            recipe.utxosWithdrawn.foldLeft(Value.zero)((s, w) => s + w._2.value)

        // Change output value is:
        // The treasury value,
        //    minus the ADA value of all withdrawn UTxOs,
        //    minus the beacon token from the treasury (which is burned),
        //    minus the fee)
        // Note that we have to explicitly set the "assets" field to empty, because
        // we will get a runtime exception if any of the multiassets are 0.
        // We can't just subtract the beacon token
        val changeValue =
            try {
                (recipe.treasuryUtxo._2._2.value
                    - valueWithdrawn
                    - Value(feeCoin))
                    .copy(assets = MultiAsset.empty)
            } catch {
                case _: IllegalArgumentException => return Left(IllegalChangeValue)
            }

        val changeOutput: TransactionOutput = TransactionOutput(
          address = recipe.changeAddress,
          value = changeValue,
          datumOption = None
        )

        val txBody =
            emptyTxBody.copy(
              inputs = Set(recipe.treasuryUtxo.utxo._1),
              outputs = recipe.utxosWithdrawn.toIndexedSeq
                  .map(_._2)
                  .map(b => Sized(b))
                  .appended(Sized(changeOutput)),
              // TODO: we set the fee to 1 ada, but this doesn't need to be
              fee = feeCoin,
              mint = Some(beaconTokenBurn)
            )

        val scalusTransaction: Transaction = Transaction(
          body = KeepRaw(txBody),
          witnessSet = TransactionWitnessSet(nativeScripts = Set(recipe.headNativeScript)),
          isValid = true,
          auxiliaryData = None
        )

        Right(FinalizationTx(treasurySpent = recipe.treasuryUtxo, tx = scalusTransaction))
    }

}
