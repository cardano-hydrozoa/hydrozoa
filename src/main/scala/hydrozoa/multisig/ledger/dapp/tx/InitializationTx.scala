package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.*
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.Token.mkHeadTokenName
import hydrozoa.multisig.ledger.dapp.tx.InitializationTx.BuildError.{OtherScalusBalancingError, OtherScalusTransactionException}
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.L1TxTypes.Initialization
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.txbuilder.*
import scalus.cardano.ledger.txbuilder.TxBuilder
import scalus.cardano.ledger.{txbuilder, *}

import scala.collection.immutable.SortedMap

final case class InitializationTx(
    treasuryProduced: TreasuryUtxo,
    override val tx: Transaction
) extends Tx

// TODO: use single seed utxo
// TODO: multiple funding utxos
// TODO: add collateral utxo outputs for each peer (how much should be in there? Upperbound: max exunits collateral)
object InitializationTx {
    final case class Recipe(
        seedUtxos: NonEmptyList[(TransactionInput, TransactionOutput)],
        initialDeposit: Coin,
        peers: NonEmptyList[VerificationKeyBytes],
        context: BuilderContext,
        changeAddress: ShelleyAddress
    )

    sealed trait ParseError

    final case class ParseConfig(
        initialL2UtxoSet: Map[TransactionInput, TransactionOutput],
        peers: NonEmptyList[VerificationKeyBytes]
    )

    // - Parse: metadata:
    //      seed utxo OutputRef,
    //      "Initialization",
    //      treasury output index,
    //      multisig regime witness output index,
    //      head address
    // - Derive: treasury token name
    // - Parse: check that the head address is what we expected (and derived from a multisig native script)
    // - Check output for head address
    // - Check: Inputs contains seed utxo (from tx metadata)
    // - Check: multisig regime witness output at head address with head token and empty datum/script + sufficient ada
    //   for fake fallback transaction (upper bound: 16KiB for fees + minAda for vote utxos).
    //   Can we do better?).
    // - Check: treasury output:
    //   - is at the head address with the head token
    //   - has well-formed datum (with correct initial kzg commitment)
    //   - contains at least total value of initial UTxO set
    def parse(
        expectedConfig: ParseConfig,
        txSerialized: Array[Byte]
    ): Either[ParseError, InitializationTx] = ???

    enum BuildError:
        case OtherScalusBalancingError(e: TxBalancingError)
        case OtherScalusTransactionException(e: TransactionException)

    def build(recipe: Recipe): Either[BuildError, InitializationTx] = {
        // Construct head native script directly from the list of peers
        val headNativeScript = HeadMultisigScript(recipe.peers)

        // singleton beacon token minted by the native script with the TN being the hash of the
        // seed utxos
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
            .withInputs((recipe.seedUtxos.toList.toSet.map(_._1)))
            .addMint(headToken)
            .attachNativeScript(headNativeScript.script, 0)
            .setAuxData(MD.apply(Initialization, headAddress))
            .addDummyVKeys(headNativeScript.numSigners)
            // Treasury Output
            .payToScript(
              address = headAddress,
              value = headValue,
              datum = datum.toData
            )
            // Change Output
            // TODO: let x be the imbalace of the transaction
            // if 0 <= x < minAda, then we should pay to treasury
            // if minAda <= x, then we should use a change output
            .addOutputs(List(Babbage(address = recipe.changeAddress, 
                value = Value.zero, 
                datumOption = None, 
                scriptRef = None)))

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
