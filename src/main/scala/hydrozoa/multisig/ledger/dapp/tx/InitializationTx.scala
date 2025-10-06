package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.*
import hydrozoa.lib.tx.ScriptSource.NativeScriptValue
import hydrozoa.lib.tx.TransactionBuilderStep.{Mint, ModifyAuxiliaryData, Send, Spend}
import hydrozoa.lib.tx.{
    ExpectedSigner,
    NativeScriptWitness,
    PubKeyWitness,
    TransactionBuilder,
    TransactionUnspentOutput,
    TxBuildError
}
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.Token.mkHeadTokenName
import hydrozoa.multisig.ledger.dapp.tx.InitializationTx.BuildError.{
    OtherScalusBalancingError,
    OtherScalusTransactionException,
    SomeBuilderError
}
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.L1TxTypes.Initialization
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import scala.collection.immutable.SortedMap
import scalus.builtin.Data.toData
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.txbuilder.*

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
        case SomeBuilderError(e: TxBuildError)
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

        //// Change Output
        //// TODO: let x be the imbalace of the transaction
        //// if 0 <= x < minAda, then we should pay to treasury
        //// if minAda <= x, then we should use a change output
        // .addOutputs(
        //  List(
        //    Babbage(
        //      address = recipe.changeAddress,
        //      value = Value.zero,
        //      datumOption = None,
        //      scriptRef = None
        //    )
        //  )
        // )

        for {

            unbalancedTx <- TransactionBuilder
                .build(
                  Mainnet,
                  recipe.seedUtxos
                      .map(utxo =>
                          Spend(
                            TransactionUnspentOutput.apply(utxo._1, utxo._2),
                            PubKeyWitness
                          )
                      )
                      .toList
                      ++ List(
                        Mint(
                          headNativeScript.script.scriptHash,
                          headTokenName,
                          1,
                          NativeScriptWitness(
                            NativeScriptValue(headNativeScript.script),
                            headNativeScript.requiredSigners.toSeq.toSet.map(ExpectedSigner(_))
                          )
                        ),
                        Send(
                          Babbage(headAddress, headValue, Some(Inline(datum.toData)), None)
                        ),
                        Send(Babbage(recipe.changeAddress, Value.zero, None, None)),
                        ModifyAuxiliaryData(_ => Some((MD.apply(Initialization, headAddress))))
                      )
                )
                .left
                .map(SomeBuilderError(_))

            // _ = println(HexUtil.encodeHexString(unbalancedTx.toCbor))

            balanced <- LowLevelTxBuilder
                .balanceFeeAndChange(
                  initial =
                      addDummyVKeys(unbalancedTx.expectedSigners.size, unbalancedTx.transaction),
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

/*

This is the sketch of what we want to have:

```scala

// Everything is resolved
case class Recipe(
                   // The only seed utxo that affects the beacon token name
                   seedUtxo: SpendOutput,
                   // Other utxo to fund the treasury, maybe empty
                   otherFundingUtxos: List[SpendOutput],
                   // NB: currently the balancing of tokens is NOT supported
                   // make a pair of commitment with its value
                   utxosL2Value: Value,
                   utxosL2Commitment: UtxoCommitment,
                   peers: NonEmptyList[VerificationKeyBytes],
                   // TODO: should be gone somewhere else
                   context: BuilderContext,
                   // If absent, the rest should go to the treasury utxo
                   changeAddress: Option[ShelleyAddress]
                 )

val collateral = ???
val fallbackFees = ???

val outputsToSpend = seedUtxo :: otherFundingUtxos

val headNativeScript = HeadMultisigScript(recipe.peers)
val beaconTokenName = mkBeaconTokenName(recipe.seedUtxos.map(_._1))
val multisigTokenName = mkMultisigTokenName(recipe.seedUtxos.map(_._1))
val beaconToken, multisigToken: MultiAsset = ???

val minTreasuryValue = utxosL2Value + beaconToken + minAda for the beacon token
val minMultisigValue = multisigToken + collateral + fallbackFees + minAda for the multisig token

val headAddress = ???
val treasuryDatum = ??? // utxosL2Commitment


// Pure building
val (unbalancedTx, vkeys: Set[VerificationKeyBytes]) = buildTransaction(
  outputsToSpend ++
  List(
    Pay(TrasactionOutput(headAddress, minTreasuryValue, treasuryDatum)),
    Pay(TrasactionOutput(headAddress, minMultisigValue, None)),
    Mint(beaconToken, headScriptCredentialWitness),
    Mint(multisigToken, headScriptCredentialWitness),
    Metadata() // additive
  ))
)

// Final step - aux data hash + probably something else

// TODO: bake it into a separate builder constructor Endo[Transaction]?
unbalancedTx = unbalancedTx.editTransaction(List(
  _.focus(_.metadata).replace(???),
  if changeAddress then _.focus(_.budy.outputs).add(???)
)

// Should handle the corner case when the change is smaller than minAda
// Should add and then remove all fake signatures under the hood
// Consider required signatures from the tx body
balancedTx = balanceFeeAndChange___(vkeys, changeOutputIdx = treasuryOutput | changeOutput)

```
 */
