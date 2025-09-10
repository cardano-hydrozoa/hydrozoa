package hydrozoa.multisig.ledger.l1.real.tx

import hydrozoa.multisig.ledger.l1.real.LedgerL1.Tx
import hydrozoa.multisig.ledger.l1.real.script.multisig.mkHeadNativeScript
import hydrozoa.multisig.ledger.l1.real.token.Token.mkHeadTokenName
import hydrozoa.multisig.ledger.l1.real.utxo.TreasuryUtxo
import hydrozoa.{VerificationKeyBytes, emptyTxBody}
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline

import scala.collection.immutable.SortedMap

final case class InitializationTx(
    treasuryProduced: TreasuryUtxo,
    headAddress: ShelleyAddress,
    override val tx: Transaction
) extends Tx

object InitializationTx {
    final case class Recipe(
        network: Network,
        seedUtxo: (TransactionInput, TransactionOutput),
        coins: BigInt,
        peers: Set[VerificationKeyBytes]
    )

    sealed trait BuildError extends Throwable
    case object IllegalChangeValue extends BuildError

    def build(recipe: Recipe): Either[BuildError, InitializationTx] = {
        // TODO: we set the fee to 1 ada, but this doesn't need to be
        val feeCoin = Coin(1_000_000)
        // Construct head native script directly from the list of peers
        val headNativeScript = mkHeadNativeScript(recipe.peers)

        // Put the head address of the native script
        val headAddress = (
          ShelleyAddress(
            network = recipe.network,
            payment = ShelleyPaymentPart.Script(headNativeScript.scriptHash),
            delegation = Null
          )
        )

        // singleton beacon token minted by the native script with the TN being the hash of the
        // seed utxo
        // TODO: factor out "mkSingleToken"
        val headTokenName = mkHeadTokenName(List(recipe.seedUtxo._1))
        val headTokenToken: MultiAsset = MultiAsset(
          SortedMap(
            headNativeScript.scriptHash -> SortedMap(headTokenName -> 1L)
          )
        )

        // Head output (L1) sits at the head address with the initial deposit from the seed utxo
        // and beacon, as well as the initial datum.
        val headValue: Value =
            Value(coin = Coin(recipe.coins.toLong), multiAsset = headTokenToken)
        val headOutput: TransactionOutput =
            TransactionOutput(
              address = headAddress,
              value = headValue,
              datumOption = Some(Inline(TreasuryUtxo.mkInitMultisigTreasuryDatum.toData))
            )

        val changeOutput: TransactionOutput = TransactionOutput(
          address = recipe.seedUtxo._2.address,
          // Change is calculated manually here as the seed output's value, minus the
          // ada put into the head, minus the fee.
          value =
              try {
                  recipe.seedUtxo._2.value -
                      Value(coin = Coin(recipe.coins.toLong)) -
                      Value(
                        coin = feeCoin
                      )
              } catch {
                  case _: IllegalArgumentException =>
                      return Left(IllegalChangeValue)
              },
          datumOption = None
        )

        val ourBody =
            emptyTxBody.copy(
              inputs = Set(recipe.seedUtxo._1),
              outputs = IndexedSeq(headOutput, changeOutput).map(Sized(_)),
              // TODO: we set the fee to 1 ada, but this doesn't need to be
              fee = feeCoin,
              mint = Some(Mint(headTokenToken))
            )

        val scalusTransaction: Transaction = Transaction(
          body = KeepRaw(ourBody),
          witnessSet = TransactionWitnessSet(nativeScripts = Set(headNativeScript)),
          isValid = true,
          auxiliaryData = None
        )

        Right(
          InitializationTx(
            headAddress = headAddress,
            treasuryProduced = TreasuryUtxo(
              headTokenName = headTokenName,
              utxo = (
                TransactionInput(
                  transactionId = scalusTransaction.id,
                  index = 0
                ),
                headOutput
              )
            ),
            tx = scalusTransaction
          )
        )
    }
}
