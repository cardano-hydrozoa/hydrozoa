package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.Token.mkHeadTokenName
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.L1TxTypes.Initialization
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import hydrozoa.*
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.txbuilder.*
import scalus.cardano.ledger.{txbuilder, *}

import scala.collection.immutable.SortedMap
import scala.util.{Failure, Success, Try}

final case class InitializationTx(
    treasuryProduced: TreasuryUtxo,
    headAddress: ShelleyAddress,
    override val tx: Transaction
) extends Tx

object InitializationTx {
    final case class Recipe(
        seedUtxo: (TransactionInput, TransactionOutput),
        coins: BigInt,
        peers: NonEmptyList[VerificationKeyBytes],
        context: BuilderContext
    )

    def build(recipe: Recipe): Either[TxBalancingError, InitializationTx] = {
        // Construct head native script directly from the list of peers
        val headNativeScript = HeadMultisigScript(recipe.peers)

        // singleton beacon token minted by the native script with the TN being the hash of the
        // seed utxo
        val headTokenName = mkHeadTokenName(List(recipe.seedUtxo._1))
        val headToken: MultiAsset = MultiAsset(
          SortedMap(
            headNativeScript.script.scriptHash -> SortedMap(headTokenName -> 1L)
          )
        )
        val headAddress: ShelleyAddress = headNativeScript.address(recipe.context.network)
        // Head output (L1) sits at the head address with the initial deposit from the seed utxo
        // and beacon, as well as the initial datum.
        val headValue: Value =
            Value(coin = Coin(recipe.coins.toLong), multiAsset = headToken)

        lazy val builder = {
            val b = recipe.context.buildNewTx
                // Treasury Output
                .payToScript(
                  address = headAddress,
                  value = headValue,
                  datum = TreasuryUtxo.mkInitMultisigTreasuryDatum.toData
                )
                // Change Output
                .payTo(address = recipe.seedUtxo._2.address, value = Value.zero)
                .selectInputs(SelectInputs.particular(Set(recipe.seedUtxo._1)))
                .addMint(headToken)
                .attachNativeScript(headNativeScript.script, 0)
                .setAuxData(MD.apply(Initialization, headAddress))
                .addDummyVKeys(headNativeScript.numSigners)

            LowLevelTxBuilder
                .balanceFeeAndChange(
                  initial = b.tx,
                  changeOutputIdx = 1,
                  protocolParams = recipe.context.protocolParams,
                  resolvedUtxo = recipe.context.utxo,
                  evaluator = recipe.context.evaluator
                )
                .map(removeDummyVKeys(headNativeScript.numSigners, _))
        }

        builder.map(tx =>
            InitializationTx(
              headAddress = headAddress,
              treasuryProduced = TreasuryUtxo(
                headTokenName = headTokenName,
                utxo = (
                  TransactionInput(
                    transactionId = tx.id,
                    index = 0
                  ),
                  tx.body.value.outputs.head.value
                )
              ),
              tx = tx
            )
        )
    }
}
