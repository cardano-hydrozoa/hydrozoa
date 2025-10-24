package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.*
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.L1TxTypes.Initialization
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import scala.collection.immutable.SortedMap
import scalus.builtin.Data.toData
import scalus.cardano.address.{Network, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.txbuilder.ScriptSource.NativeScriptValue
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, ModifyAuxiliaryData, Send, Spend}
import scalus.cardano.txbuilder.{
    NativeScriptWitness,
    PubKeyWitness,
    SomeBuildError,
    TransactionBuilder,
    TransactionUnspentOutput
}

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
        network: Network,
        protocolParams: ProtocolParams,
        evaluator: PlutusScriptEvaluator,
        validators: Seq[Validator],
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

    def build(recipe: Recipe): Either[SomeBuildError, InitializationTx] = {
        ////////////////////////////////////////////////////////////
        // Data extraction

        // Construct head native script directly from the list of peers
        val headNativeScript = HeadMultisigScript(recipe.peers)

        // singleton beacon token minted by the native script with the TN being the hash of the
        // seed utxos
        // FIXME: This should just be a single seed UTxO
        val headTokenName = CIP67.TokenNames(recipe.seedUtxos.map(_._1).head).headTokenName
        val headToken: MultiAsset = MultiAsset(
          SortedMap(
            headNativeScript.script.scriptHash -> SortedMap(headTokenName -> 1L)
          )
        )
        val headAddress: ShelleyAddress = headNativeScript.mkAddress(recipe.network)
        // Head output (L1) sits at the head address with the initial deposit from the seed utxo
        // and beacon, as well as the initial datum.
        val headValue: Value =
            Value(coin = recipe.initialDeposit, assets = headToken)

        val datum = TreasuryUtxo.mkInitMultisigTreasuryDatum

        /////////////////////////////////////////////////////////
        // Steps
        val spendSeedUtxos: Seq[Spend] = recipe.seedUtxos
            .map(utxo =>
                Spend(
                  TransactionUnspentOutput.apply(utxo._1, utxo._2),
                  PubKeyWitness
                )
            )
            .toList

        val mintBeaconToken = Mint(
          headNativeScript.script.scriptHash,
          headTokenName,
          1,
          NativeScriptWitness(
            NativeScriptValue(headNativeScript.script),
            headNativeScript.requiredSigners
          )
        )

        val createTreasury: Send = Send(
          Babbage(headAddress, headValue, Some(Inline(datum.toData)), None)
        )

        val createChangeOutput = Send(Babbage(recipe.changeAddress, Value.zero, None, None))

        val modifyAuxiliaryData =
            ModifyAuxiliaryData(_ => Some((MD.apply(Initialization, headAddress))))

        val steps = spendSeedUtxos
            .appended(mintBeaconToken)
            .appended(createTreasury)
            .appended(createChangeOutput)
            .appended(modifyAuxiliaryData)

        ////////////////////////////////////////////////////////////
        // Build and finalize
        for {

            unbalanced <- TransactionBuilder
                .build(
                  recipe.network,
                  steps
                )

            finalized <- unbalanced
                .finalizeContext(
                  protocolParams = recipe.protocolParams,
                  diffHandler = new ChangeOutputDiffHandler(
                    recipe.protocolParams,
                    1
                  ).changeOutputDiffHandler,
                  evaluator = recipe.evaluator,
                  validators = recipe.validators
                )

        } yield (InitializationTx(
          treasuryProduced = TreasuryUtxo(
            headTokenName = headTokenName,
            txId = TransactionInput(
              transactionId = finalized.transaction.id,
              index = 0
            ),
            address = headAddress,
            datum = datum,
            value = headValue
          ),
          tx = finalized.transaction
        ))
    }
}
