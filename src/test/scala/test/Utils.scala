package test
import hydrozoa.multisig.ledger.VirtualLedgerM
import hydrozoa.multisig.ledger.dapp.tx.TxTiming
import scala.language.postfixOps
import scalus.cardano.address.Network
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
import scalus.uplc.eval.ExBudget
import test.TestPeer.Alice

val blockfrost544Params: ProtocolParams = ProtocolParams.fromBlockfrostJson(
  this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
)

val costModels = blockfrost544Params.costModels

// Individual parameters for Recipe constructors (replacing BuilderContext)
/** WARNING: Use this. Don't use Network.Testnet. Scalus uses Mainnet in its test utils.
  *
  * WARNING: Don't use this if you can use head config.
  */
val testNetwork: Network = Mainnet
val testProtocolParams: ProtocolParams = blockfrost544Params

private def slotConfig(network: Network): SlotConfig = network match {
    case Network.Testnet  => SlotConfig.preprod
    case Network.Mainnet  => SlotConfig.mainnet
    case Network.Other(v) => throw RuntimeException("This network is not supported in tests")
}

private val evaluator = PlutusScriptEvaluator(
  slotConfig = slotConfig(testNetwork),
  initialBudget = ExBudget.enormous,
  protocolMajorVersion = MajorProtocolVersion.plominPV,
  costModels = costModels
)

val testEvaluator: PlutusScriptEvaluator = evaluator

// TODO: this should be removed at some point in favor of HeadConfig
val testTxBuilderCardanoInfo: CardanoInfo = CardanoInfo(
  protocolParams = testProtocolParams,
  slotConfig = slotConfig(testNetwork),
  network = testNetwork
)

val testTxTiming: TxTiming =
    TxTiming.default(testTxBuilderCardanoInfo.slotConfig)

def testVirtualLedgerConfig(slot: SlotNo): VirtualLedgerM.Config =
    VirtualLedgerM.Config(testTxBuilderCardanoInfo)

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
