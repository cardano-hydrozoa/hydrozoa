package test
import hydrozoa.multisig.ledger.VirtualLedgerM
import scala.language.postfixOps
import scalus.cardano.address.Network
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.txbuilder.Environment
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
import scalus.uplc.eval.ExBudget
import test.TestPeer.Alice

val blockfrost544Params: ProtocolParams = ProtocolParams.fromBlockfrostJson(
  this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
)

val costModels = blockfrost544Params.costModels

// Individual parameters for Recipe constructors (replacing BuilderContext)
/** WARNING: Use this. Don't use Network.Testnet. Scalus uses Mainnet in its test utils.
  */
val testNetwork: Network = Mainnet
val testProtocolParams: ProtocolParams = blockfrost544Params

def slotConfig(network: Network): SlotConfig = network match {
    case Network.Testnet  => SlotConfig.Preprod
    case Network.Mainnet  => SlotConfig.Mainnet
    case Network.Other(v) => throw RuntimeException("This network is not supported in tests")
}

val evaluator = PlutusScriptEvaluator(
  slotConfig = slotConfig(testNetwork),
  initialBudget = ExBudget.enormous,
  protocolMajorVersion = MajorProtocolVersion.plominPV,
  costModels = costModels
)

val testEvaluator: PlutusScriptEvaluator = evaluator

val nonSigningValidators: Seq[Validator] =
    // These validators are all the ones from the CardanoMutator that could be checked on an unsigned transaction
    List(
      EmptyInputsValidator,
      InputsAndReferenceInputsDisjointValidator,
      AllInputsMustBeInUtxoValidator,
      ValueNotConservedUTxOValidator,
      // VerifiedSignaturesInWitnessesValidator,
      // MissingKeyHashesValidator
      MissingOrExtraScriptHashesValidator,
      TransactionSizeValidator,
      FeesOkValidator,
      OutputsHaveNotEnoughCoinsValidator,
      OutputsHaveTooBigValueStorageSizeValidator,
      OutsideValidityIntervalValidator,
      OutsideForecastValidator
    )

val testTxBuilderEnvironment: Environment = CardanoInfo(
  protocolParams = testProtocolParams,
  slotConfig = slotConfig(testNetwork),
  network = testNetwork
)

def testVirtualLedgerConfig(slot: SlotNo): VirtualLedgerM.Config = VirtualLedgerM.Config(
  slotConfig = testTxBuilderEnvironment.slotConfig,
  slot = slot,
  protocolParams = testTxBuilderEnvironment.protocolParams,
  network = testNetwork
)

// Get the minAda for an Ada only pubkey utxo
def minPubkeyAda(params: ProtocolParams = blockfrost544Params): Coin = {
    ensureMinAda(
      TransactionOutput.Babbage(Alice.address(testNetwork), Value.zero, None, None),
      blockfrost544Params
    ).value.coin
}

def sumUtxoValues(utxos: Seq[Utxo]): Value =
    utxos.map(_._2.value).foldLeft(Value.zero)((acc: Value, v: Value) => acc + v)

extension [E, A](e: Either[E, A])
    def get: A = e match {
        case Right(a) => a
    }
