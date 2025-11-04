package test

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.tx.{Tx}
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import monocle.*
import monocle.syntax.all.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.const
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.Network.Testnet
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.txbuilder.{Datum, Environment}
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
import scalus.prelude.Option as SOption
import scalus.uplc.eval.ExBudget
import test.Generators.Hydrozoa.{genAdaOnlyPubKeyUtxo, genFakeMultisigWitnessUtxo}
import test.TestPeer.Alice

import scala.language.postfixOps

val blockfrost544Params: ProtocolParams = ProtocolParams.fromBlockfrostJson(
  this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
)

val costModels = blockfrost544Params.costModels

// Individual parameters for Recipe constructors (replacing BuilderContext)
val testNetwork: Network = Testnet
val testProtocolParams: ProtocolParams = blockfrost544Params

def slotConfig(network: Network): SlotConfig = network match {
    case Network.Testnet => SlotConfig.Preprod
    case Network.Mainnet => SlotConfig.Mainnet
    case Network.Other(v) => throw RuntimeException("This network is not supported in tests")
}

val evaluator = PlutusScriptEvaluator(
    slotConfig = slotConfig(testNetwork),
    initialBudget = ExBudget.enormous,
    protocolMajorVersion = MajorProtocolVersion.plominPV,
    costModels = costModels
)

val testEvaluator: PlutusScriptEvaluator = evaluator

val testValidators: Seq[Validator] =
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

val testTxBuilderEnvironment: Environment = Environment(
  protocolParams = testProtocolParams,
  slotConfig = slotConfig(testNetwork),
  evaluator = testEvaluator,
  network = testNetwork,
  era = Era.Conway
)





// Get the minAda for an Ada only pubkey utxo
def minPubkeyAda(params: ProtocolParams = blockfrost544Params) = {
    val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get.focus(_._2.value.coin.value).replace(0L)
    ensureMinAda(utxo._2, blockfrost544Params).value.coin
}

def sumUtxoValues(utxos: Seq[(TransactionInput, TransactionOutput)]): Value =
    utxos.map(_._2.value).foldLeft(Value.zero)((acc: Value, v: Value) => acc + v)

extension [E,A](e : Either[E,A])
    def get: A = e match {
      case Right(a) => a
    }

