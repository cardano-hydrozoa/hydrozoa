package test

import hydrozoa.lib.cardano.scalus.ledger.txbuilder.setMinAda
import org.scalacheck.*
import org.scalacheck.Gen.{const, posNum}
import scalus.builtin.ByteString
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.txbuilder.{BuilderContext, UtxoProvider}
import scalus.ledger.api.MajorProtocolVersion
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
import scalus.ledger.babbage.ProtocolParams
import scalus.uplc.eval.ExBudget
import upickle.default.read

import scala.language.postfixOps

val blockfrost544Params: ProtocolParams = read[ProtocolParams](
  this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
)(using ProtocolParams.blockfrostParamsRW)

val costModels = CostModels.fromProtocolParams(blockfrost544Params)

val evaluator = PlutusScriptEvaluator(
  SlotConfig.Mainnet,
  initialBudget = ExBudget.enormous,
  protocolMajorVersion = MajorProtocolVersion.plominPV,
  costModels = costModels
)

def unsignedTxBuilderContext(utxo: UTxO): BuilderContext = {

    BuilderContext(
      protocolParams = blockfrost544Params,
      evaluator = evaluator,
      network = Mainnet,
      utxoProvider = UtxoProvider.from(utxo),
      validators =
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
          ),
      signingKeys = Map.empty
    )
}

val genTxId: Gen[TransactionInput] =
    for {
        txId <- genByteStringOfN(32).map(TransactionHash.fromByteString)
        index <- posNum[Int] // we subtract one below to get a non-negative

    } yield TransactionInput(transactionId = txId, index = index - 1)

val genAddrKeyHash: Gen[AddrKeyHash] =
    genByteStringOfN(28).map(AddrKeyHash.fromByteString)

val genPolicyId: Gen[PolicyId] = genByteStringOfN(28).map(ScriptHash.fromByteString)

def genPubkeyAddr(
    network: Network = Mainnet,
    delegation: ShelleyDelegationPart = ShelleyDelegationPart.Null
): Gen[ShelleyAddress] =
    genAddrKeyHash.flatMap(akh =>
        ShelleyAddress(network = network, payment = Key(akh), delegation = delegation)
    )

/** Generate a positive Ada value */
val genAdaOnlyValue: Gen[Value] =
    for {
        coin <- Gen.posNum[Long]
    } yield Value(Coin(coin))

/** Ada-only pub key utxo from the given peer, at least minAda, random tx id, random index, no
  * datum, no script ref
  */
def genAdaOnlyPubKeyUtxo(
    peer: TestPeer,
    params: ProtocolParams = blockfrost544Params
): Gen[(TransactionInput, Babbage)] =
    for {
        txId <- genTxId
        value <- genAdaOnlyValue
    } yield (
      txId,
      setMinAda(
        Babbage(
          address = peer.address,
          value = value,
          datumOption = None,
          scriptRef = None
        ),
        params
      )
    )

def sumUtxoValues(utxos: Seq[(TransactionInput, TransactionOutput)]): Value =
    utxos.map(_._2.value).foldLeft(Value.zero)((acc: Value, v: Value) => acc + v)
