package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.*
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.L1TxTypes.Initialization
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.Network.Testnet
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{Network, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.txbuilder.ScriptSource.NativeScriptValue
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, ModifyAuxiliaryData, Send, Spend}
import scalus.cardano.txbuilder.*

import scala.collection.immutable.SortedMap

final case class InitializationTx(
    treasuryProduced: TreasuryUtxo,
    multisigRegimeUtxo : TransactionUnspentOutput, // TODO: newtype
    override val tx: Transaction
) extends Tx

// TODO: use single seed utxo
// TODO: multiple funding utxos
// TODO: add collateral utxo outputs for each peer (how much should be in there? Upperbound: max exunits collateral)
object InitializationTx {

    final case class Recipe(
                               private val seedUtxo0: TransactionInput,
                               // Must contain the seedUtxo TransactionInput
                               private val spentUtxos0: NonEmptyList[TransactionUnspentOutput],
                               initialDeposit: Coin,
                               peers: NonEmptyList[VerificationKeyBytes],
                               env: Environment,
                               validators: Seq[Validator],
                               changeAddress: ShelleyAddress
                           ) {
        def seedUtxo: TransactionInput = seedUtxo0
        def spentUtxos: NonEmptyList[TransactionUnspentOutput] = spentUtxos0
    }

    object Recipe {
        // TODO: Better error type
        def apply(
                        seedUtxo: TransactionInput,
                        // Must contain the seedUtxo TransactionInput
                        spentUtxos: NonEmptyList[TransactionUnspentOutput],
                        initialDeposit: Coin,
                        peers: NonEmptyList[VerificationKeyBytes],
                        env: Environment,
                        validators: Seq[Validator],
                        changeAddress: ShelleyAddress
                    ): Option[InitializationTx.Recipe] =
            if spentUtxos.map(_.input).toList.contains(seedUtxo)
            then Some(new Recipe(seedUtxo, spentUtxos, initialDeposit, peers, env, validators, changeAddress))
            else None
    }


    /*
    Initialization tx spec:
    - Spend configured inputs, ensuring that the configured seed utxo is among them.
    - Mint the treasury and multisig regime tokens.Produce the initial treasury with at least the configured value, plus the treasury token.
    - Produce the multisig regime utxo with the fallback deposit (i.e. sufficient funds to cover the fallback tx), plus the multisig regime token.
    - Cover the tx fee using the treasury output in the diff handler.
    - Fail if there aren't enough spent funds to cover all these requirements.
Fallback tx spec:

    Spend both the treasury and multisig regime utxos.

Burn the multisig regime token.Mint N+1 vote tokens.Produce a rule-based treasury utxo, containing all treasury utxo funds (i.e. don't deduct the fee)Produce default vote utxo with minimum ADA, plus a vote token.Produce one vote utxo per peer, containing minimum ADA plus a configured allowance for one tally tx fee, plus a vote token.Produce one collateral utxo per peer, containing minimum ADA plus a configured allowance for one vote tx fee.Cover the tx fee and ADA for all non-treasury outputs using funds from the multisig regime utxo.
     */




    def build(recipe: Recipe): Either[SomeBuildError, InitializationTx] = {
        ////////////////////////////////////////////////////////////
        // Data extraction

        // Construct head native script directly from the list of peers
        val headNativeScript = HeadMultisigScript(recipe.peers)

        // singleton beacon token minted by the native script with the TN being the hash of the
        // seed utxos
        val headTokenName = CIP67.TokenNames(recipe.seedUtxo).headTokenName
        val headToken: MultiAsset = MultiAsset(
          SortedMap(
            headNativeScript.script.scriptHash -> SortedMap(headTokenName -> 1L)
          )
        )
        
        val headAddress: ShelleyAddress = headNativeScript.mkAddress(recipe.env.network)
        // Head output (L1) sits at the head address with the initial deposit from the seed utxo
        // and beacon, as well as the initial datum.
        val headValue: Value =
            Value(coin = recipe.initialDeposit, assets = headToken)

        val mrTokenName = CIP67.TokenNames(recipe.seedUtxo).multisigRegimeTokenName
        val mrToken: MultiAsset = MultiAsset(SortedMap(
          headNativeScript.policyId -> SortedMap(mrTokenName -> 1L)
        ))

        // Lovelace per tx byte (a): 44 Lovelace per tx (b): 155381 Max tx bytes: 16 * 1024 = 16384
        //Therefore, max non-Plutus tx fee: 16 * 1024 * 44 + 155381 = 720896 + 155381 = 876277
        // (this is the deposit to cover the fallback transaction fee)
        val mrValue = Value(maxNonPlutusTxFee(recipe.env.protocolParams), mrToken)

        val datum = TreasuryUtxo.mkInitMultisigTreasuryDatum

        /////////////////////////////////////////////////////////
        // Steps
        val spendAllUtxos: Seq[Spend] = recipe.spentUtxos
            .map(utxo =>
                Spend(
                  TransactionUnspentOutput.apply(utxo._1, utxo._2),
                  PubKeyWitness
                )
            ).toList
        
        val mintBeaconToken = Mint(
          headNativeScript.script.scriptHash,
          headTokenName,
          1,
          NativeScriptWitness(
            NativeScriptValue(headNativeScript.script),
            headNativeScript.requiredSigners
          )
        )
        
        val mintMRToken = Mint(
          headNativeScript.script.scriptHash,
          mrTokenName,
          1,
          NativeScriptWitness(
            NativeScriptValue(headNativeScript.script),
            headNativeScript.requiredSigners
          )
        )
        val hmrwOutput =Babbage(headAddress, mrValue, None, None).ensureMinAda(recipe.env.protocolParams)


        val createTreasury: Send = Send(
          Babbage(headAddress, headValue, Some(Inline(datum.toData)), None)
        )

        val createChangeOutput = Send(Babbage(recipe.changeAddress, Value.zero, None, None))

        val modifyAuxiliaryData =
            ModifyAuxiliaryData(_ => Some((MD.apply(Initialization, headAddress))))

        val steps = spendAllUtxos
            .appended(mintBeaconToken)
            .appended(mintMRToken)
            .appended(createTreasury)
            .appended(Send(hmrwOutput))
            .appended(createChangeOutput)
            .appended(modifyAuxiliaryData)

        ////////////////////////////////////////////////////////////
        // Build and finalize
        for {

            unbalanced <- TransactionBuilder
                .build(
                  recipe.env.network,
                  steps
                )

            finalized <- unbalanced
                .finalizeContext(
                  protocolParams = recipe.env.protocolParams,
                  diffHandler = new ChangeOutputDiffHandler(
                    recipe.env.protocolParams,
                    2
                  ).changeOutputDiffHandler,
                  evaluator = recipe.env.evaluator,
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
          tx = finalized.transaction,
          multisigRegimeUtxo = TransactionUnspentOutput(TransactionInput(finalized.transaction.id, 1), hmrwOutput)
        ))
    }



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

}
