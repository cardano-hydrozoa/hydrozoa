package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.*
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.Token.mkHeadTokenName
import hydrozoa.multisig.ledger.dapp.tx.InitializationTx.BuildError.{
    OtherScalusBalancingError,
    OtherScalusTransactionException
}
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.L1TxTypes.Initialization
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.txbuilder.*
import scalus.cardano.ledger.{txbuilder, *}

import scala.collection.immutable.SortedMap

final case class InitializationTx(
    treasuryProduced: TreasuryUtxo,
    override val tx: Transaction
) extends Tx

object InitializationTx {
    final case class Recipe(
        seedUtxos: NonEmptyList[(TransactionInput, TransactionOutput)],
        initialDeposit: Coin,
        peers: NonEmptyList[VerificationKeyBytes],
        context: BuilderContext,
        changeAddress: ShelleyAddress
    )

    enum BuildError:
        case OtherScalusBalancingError(e: TxBalancingError)
        case OtherScalusTransactionException(e: TransactionException)

    def build(recipe: Recipe): Either[BuildError, InitializationTx] = {
        // Construct head native script directly from the list of peers
        val headNativeScript = HeadMultisigScript(recipe.peers)

        // singleton beacon token minted by the native script with the TN being the hash of the
        // seed utxo
        val headTokenName = mkHeadTokenName(recipe.seedUtxos.map(_._1))
        val headToken: MultiAsset = MultiAsset(
          SortedMap(
            headNativeScript.script.scriptHash -> SortedMap(headTokenName -> 1L)
          )
        )
        val headAddress: ShelleyAddress = headNativeScript.address(recipe.context.network)
        // Head output (L1) sits at the head address with the initial deposit from the seed utxo
        // and beacon, as well as the initial datum.
        val headValue: Value =
            Value(coin = recipe.initialDeposit, multiAsset = headToken)

        val datum = TreasuryUtxo.mkInitMultisigTreasuryDatum

        val b = recipe.context.buildNewTx
            // Treasury Output
            .payToScript(
              address = headAddress,
              value = headValue,
              datum = datum.toData
            )
            // Change Output
            .payTo(address = recipe.changeAddress, value = Value.zero)
            .selectInputs(SelectInputs.particular(recipe.seedUtxos.toList.toSet.map(_._1)))
            .addMint(headToken)
            .attachNativeScript(headNativeScript.script, 0)
            .setAuxData(MD.apply(Initialization, headAddress))
            .addDummyVKeys(headNativeScript.numSigners)

        for {
            balanced <- LowLevelTxBuilder
                .balanceFeeAndChange(
                  initial = b.tx,
                  changeOutputIdx = 1,
                  protocolParams = recipe.context.protocolParams,
                  resolvedUtxo = recipe.context.utxo,
                  evaluator = recipe.context.evaluator
                )
                .map(removeDummyVKeys(headNativeScript.numSigners, _))
                .left
                .map(OtherScalusBalancingError(_))
            validated <- recipe.context
                .validate(balanced)
                .left
                .map(OtherScalusTransactionException(_))
        } yield (InitializationTx(
          treasuryProduced = TreasuryUtxo(
            headTokenName = headTokenName,
            txId = TransactionInput(
              transactionId = validated.id,
              index = 0
            ),
            addr = headAddress,
            datum = datum,
            value = headValue
          ),
          tx = validated
        ))
    }
}
