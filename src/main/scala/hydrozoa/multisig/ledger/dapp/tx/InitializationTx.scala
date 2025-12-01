package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.*
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.L1TxTypes.Initialization
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import hydrozoa.rulebased.ledger.dapp.script.plutus.{DisputeResolutionScript, RuleBasedTreasuryScript}
import scalus.builtin.ByteString

import scala.collection.immutable.SortedMap
import scalus.builtin.ToData.toData
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.txbuilder.ScriptSource.NativeScriptValue
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, ModifyAuxiliaryData, Send, Spend}

final case class InitializationTx(
    treasuryProduced: TreasuryUtxo,
    resultingConfig: Tx.Builder.Config,
    override val resolvedUtxos: ResolvedUtxos,
    override val tx: Transaction
) extends Tx,
      HasResolvedUtxos

object InitializationTx {

    def build(recipe: Recipe): Either[SomeBuildError, InitializationTx] = {
        ////////////////////////////////////////////////////////////
        // Data extraction
        import recipe.*
        import tokenNames.*

        val headAddress: ShelleyAddress = headNativeScript.mkAddress(env.network)
        val changeAddress = ShelleyAddress(env.network, changePP, Null)

        val mrTokenName = CIP67.TokenNames(recipe.spentUtxos.seedUtxo.input).multisigRegimeTokenName
        val mrToken: MultiAsset = MultiAsset(
          SortedMap(
            headNativeScript.policyId -> SortedMap(multisigRegimeTokenName -> 1L)
          )
        )

        // Lovelace per tx byte (a): 44 Lovelace per tx (b): 155381 Max tx bytes: 16 * 1024 = 16384
        // Therefore, max non-Plutus tx fee: 16 * 1024 * 44 + 155381 = 720896 + 155381 = 876277
        // (this is the deposit to cover the fallback transaction fee)
        val mrValue = Value(recipe.hmrwCoin, mrToken)

        /////////////////////////////////////////////////////////
        // Steps
        val spendAllUtxos: Seq[Spend] = Seq(Spend(recipe.spentUtxos.seedUtxo)) ++
            recipe.spentUtxos.fundingUtxos
                .map(utxo =>
                    Spend(
                      TransactionUnspentOutput.apply(utxo._1, utxo._2),
                      PubKeyWitness
                    )
                )
                .toList

        val mintTreasuryToken = Mint(
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
          headNativeScript.witness
        )
        val hmrwOutput =
            Babbage(headAddress, mrValue, None, Some(ScriptRef(headNativeScript.script)))
                .ensureMinAda(recipe.env.protocolParams)

        val createTreasury: Send = Send(
          Babbage(
            headNativeScript.mkAddress(env.network),
            value = Value(
              initialDeposit,
              MultiAsset(SortedMap(headNativeScript.policyId -> SortedMap(headTokenName -> 1L)))
            ),
            datumOption = Some(Inline(TreasuryUtxo.mkInitMultisigTreasuryDatum.toData))
          )
        )

        val createChangeOutput = Send(Babbage(changeAddress, Value.zero, None, None))

        val addMetadata =
            ModifyAuxiliaryData(_ => Some((MD.apply(Initialization, headAddress))))

        val steps = spendAllUtxos
            :+ mintTreasuryToken
            :+ mintMRToken
            :+ createTreasury
            :+ Send(hmrwOutput)
            :+ createChangeOutput
            :+ addMetadata

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
                  evaluator = recipe.evaluator,
                  validators = recipe.validators
                )

        } yield InitializationTx(
          treasuryProduced = TreasuryUtxo(
            treasuryTokenName = headTokenName,
            utxoId = TransactionInput(
              transactionId = finalized.transaction.id,
              index = 0
            ),
            address = headAddress,
            datum = TreasuryUtxo.mkInitMultisigTreasuryDatum,
            value = createTreasury.output.value
          ),
          tx = finalized.transaction,
          resultingConfig = Tx.Builder.Config(
            headNativeScript = headNativeScript,
            headNativeScriptReferenceInput =
                TransactionUnspentOutput(TransactionInput(finalized.transaction.id, 1), hmrwOutput),
            tokenNames = tokenNames,
            env = env,
            evaluator = recipe.evaluator,
            validators = validators,
            disputeResolutionPaymentPart =
              ShelleyPaymentPart.Script(Hash(ByteString.fromArray(DisputeResolutionScript.getScriptHash))),
            disputeTreasuryPaymentPart =
              ShelleyPaymentPart.Script(Hash(ByteString.fromArray(RuleBasedTreasuryScript.getScriptHash)))
          ),
          resolvedUtxos = finalized.resolvedUtxos
        )
    }

    final case class SpentUtxos(
        seedUtxo: TransactionUnspentOutput,
        fundingUtxos: List[TransactionUnspentOutput]
    ) {
        def all: NonEmptyList[TransactionUnspentOutput] = NonEmptyList(seedUtxo, fundingUtxos)
    }

    final case class Recipe(
        spentUtxos: SpentUtxos,
        headNativeScript: HeadMultisigScript,
        initialDeposit: Coin,
        tokenNames: TokenNames,
        // The amount of coin to cover the minAda for the vote UTxOs and collateral
        // utxos in the fallback transaction (inclusive of the max fallback tx fee)
        hmrwCoin: Coin,
        env: Environment,
        validators: Seq[Validator],
        changePP: ShelleyPaymentPart,
        evaluator: PlutusScriptEvaluator
    ) {}

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
