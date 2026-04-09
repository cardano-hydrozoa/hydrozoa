package hydrozoa.rulebased.ledger.l1.script.plutus

import cats.data.NonEmptyList
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.{HydrozoaBlueprint, ScriptReferenceUtxos}
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import org.scalatest.funsuite.AnyFunSuite
import scala.annotation.nowarn
import scalus.cardano.ledger.rules.{Context, State}
import scalus.cardano.ledger.{Coin, ScriptRef, TransactionHash, TransactionInput, TransactionOutput, Utxo, Value}
import test.{SeedPhrase, TestPeers, TransactionChain}

@nowarn("msg=unused value")
class DeployTxTest extends AnyFunSuite {

    test("Deploy RuleBasedTreasuryScript to L1 mock") {
        // 1. Initialize a fresh L1 mock using test sauce seed phrase
        val seedPhrase = SeedPhrase.Yaci
        val network = CardanoNetwork.Mainnet
        val testPeers = TestPeers(seedPhrase, network, 1)

        val initialState = State()
        val context = Context.testMainnet()

        // Get the first peer's address and create a UTXO with funds
        import hydrozoa.multisig.consensus.peer.HeadPeerNumber
        val fundingAddress = testPeers.addressFor(HeadPeerNumber(0))
        val fundingValue = Value(Coin(50_000_000)) // 50 ADA for fees and min ADA
        val fundingTxInput = TransactionInput(TransactionHash.fromHex("0" * 64), 0)
        val fundingUtxo = Utxo(
          fundingTxInput,
          TransactionOutput.Babbage(
            address = fundingAddress,
            value = fundingValue,
            datumOption = None,
            scriptRef = None
          )
        )

        // Add funding UTXO to the ledger state
        val stateWithFunds =
            initialState.copy(utxos = initialState.utxos + (fundingTxInput -> fundingUtxo.output))

        // 2. Get the treasury script from HydrozoaBlueprint and wrap it in ScriptRef
        val treasuryScript = RuleBasedTreasuryScript.compiledPlutusV3Program.script
        val scriptRef = ScriptRef(treasuryScript)

        // 3. Build deploy transaction
        val deployTxBuilder = DeployTxOps.Build(
          utxosToSpend = NonEmptyList.one(fundingUtxo),
          scriptToDeploy = scriptRef
        )

        given CardanoNetwork.Section = network

        val deployTxResult = deployTxBuilder.result

        assert(deployTxResult.isRight, s"Deploy tx build failed: $deployTxResult")
        val deployTx = deployTxResult.toOption.get

        // 4. Sign the transaction using the test peer's wallet
        val wallet = testPeers.walletFor(HeadPeerNumber(0))
        val signedTx = deployTx.tx.attachVKeyWitnesses(List(wallet.mkVKeyWitness(deployTx.tx)))

        println(s"${HexUtil.encodeHexString(signedTx.toCbor)}")

        // 5. Submit the tx to the L1 mock and observe the ref utxo produced
        val finalStateResult = TransactionChain.foldTxChain(Seq(signedTx))(
          stateWithFunds,
          context = context
        )

        assert(finalStateResult.isRight, s"Transaction execution failed: $finalStateResult")
        val finalState = finalStateResult.toOption.get

        // 6. Get the ref UTXO and parse it into ScriptReferenceUtxos
        val refUtxoOutput = finalState.utxos.get(deployTx.refScriptUtxo)
        assert(
          refUtxoOutput.isDefined,
          s"Reference script UTXO not found at ${deployTx.refScriptUtxo}"
        )

        val refUtxo = Utxo(deployTx.refScriptUtxo, refUtxoOutput.get)
        val treasuryRefUtxoResult = ScriptReferenceUtxos.TreasuryScriptUtxo(network, refUtxo)
        assert(
          treasuryRefUtxoResult.isRight,
          s"Failed to parse treasury ref UTXO: $treasuryRefUtxoResult"
        )

        // Verify the script hash matches
        val scriptHash = refUtxo.output.scriptRef.get.script.scriptHash
        assert(
          scriptHash == HydrozoaBlueprint.treasuryScriptHash,
          "Script hash mismatch"
        )
    }

    test("Deploy DisputeResolutionScript to L1 mock") {
        // 1. Initialize a fresh L1 mock using test sauce seed phrase
        val seedPhrase = SeedPhrase.Yaci
        val network = CardanoNetwork.Mainnet
        val testPeers = TestPeers(seedPhrase, network, 1)

        val initialState = State()
        val context = Context.testMainnet()

        // Get the first peer's address and create a UTXO with funds
        import hydrozoa.multisig.consensus.peer.HeadPeerNumber
        val fundingAddress = testPeers.addressFor(HeadPeerNumber(0))
        val fundingValue = Value(Coin(50_000_000)) // 50 ADA for fees and min ADA
        val fundingTxInput = TransactionInput(TransactionHash.fromHex("0" * 64), 0)
        val fundingUtxo = Utxo(
          fundingTxInput,
          TransactionOutput.Babbage(
            address = fundingAddress,
            value = fundingValue,
            datumOption = None,
            scriptRef = None
          )
        )

        // Add funding UTXO to the ledger state
        val stateWithFunds =
            initialState.copy(utxos = initialState.utxos + (fundingTxInput -> fundingUtxo.output))

        // 2. Get the dispute script from HydrozoaBlueprint and wrap it in ScriptRef
        val disputeScript = DisputeResolutionScript.compiledPlutusV3Program.script
        val scriptRef = ScriptRef(disputeScript)

        // 3. Build deploy transaction
        val deployTxBuilder = DeployTxOps.Build(
          utxosToSpend = NonEmptyList.one(fundingUtxo),
          scriptToDeploy = scriptRef
        )

        given CardanoNetwork.Section = network

        val deployTxResult = deployTxBuilder.result

        assert(deployTxResult.isRight, s"Deploy tx build failed: $deployTxResult")
        val deployTx = deployTxResult.toOption.get

        // 4. Sign the transaction using the test peer's wallet
        val wallet = testPeers.walletFor(HeadPeerNumber(0))
        val signedTx = deployTx.tx.attachVKeyWitnesses(List(wallet.mkVKeyWitness(deployTx.tx)))

        println(s"${HexUtil.encodeHexString(signedTx.toCbor)}")

        // 5. Submit the tx to the L1 mock and observe the ref utxo produced
        val finalStateResult = TransactionChain.foldTxChain(Seq(signedTx))(
          stateWithFunds,
          context = context
        )

        assert(finalStateResult.isRight, s"Transaction execution failed: $finalStateResult")
        val finalState = finalStateResult.toOption.get

        // 6. Get the ref UTXO and parse it into ScriptReferenceUtxos
        val refUtxoOutput = finalState.utxos.get(deployTx.refScriptUtxo)
        assert(
          refUtxoOutput.isDefined,
          s"Reference script UTXO not found at ${deployTx.refScriptUtxo}"
        )

        val refUtxo = Utxo(deployTx.refScriptUtxo, refUtxoOutput.get)
        val disputeRefUtxoResult = ScriptReferenceUtxos.DisputeScriptUtxo(network, refUtxo)
        assert(
          disputeRefUtxoResult.isRight,
          s"Failed to parse dispute ref UTXO: $disputeRefUtxoResult"
        )

        // Verify the script hash matches
        val scriptHash = refUtxo.output.scriptRef.get.script.scriptHash
        assert(
          scriptHash == HydrozoaBlueprint.disputeScriptHash,
          "Script hash mismatch"
        )
    }
}
