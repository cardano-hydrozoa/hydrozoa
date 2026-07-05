package hydrozoa.multisig.ledger.eutxol2

import hydrozoa.multisig.ledger.eutxol2.L2TxFixtures.*
import hydrozoa.multisig.ledger.eutxol2.tx.L2Tx
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.NativeScriptWitness
import scalus.cardano.txbuilder.TransactionBuilderStep.Mint
import scalus.uplc.builtin.ByteString

/** Scenario tests for [[HydrozoaTransactionMutator.transit]] over the two compartments, with the
  * fixture's single-key native minting policy. The rejection cases pin the core property of the
  * design: main-compartment (L1-native) tokens cannot be minted, burned, or smuggled out of the
  * overlay, purely by the L1-projection conservation arithmetic — no policy-id checks exist
  * anywhere.
  */
class TransientTokensMutatorTest extends AnyFunSuite {

    private val demoAsset = AssetName(ByteString.fromString("DEMO"))

    private def demoBundle(quantity: Long): MultiAsset =
        MultiAsset.asset(nativeMintPolicyId, demoAsset, quantity)

    private def mintDemo(quantity: Long): Mint =
        Mint(
          nativeMintPolicyId,
          demoAsset,
          quantity,
          NativeScriptWitness.attached(nativeMintScript)
        )

    private def sendToPeer(value: Value, marker: Int = 2): (TransactionOutput, Int) =
        (Babbage(peerAddress, value), marker)

    /** Parse the signed transaction and run the mutator over `state`. */
    private def applyTransit(
        state: Compartments,
        tx: Transaction
    ): Either[String | TransactionException, (L2Tx, Compartments)] =
        for {
            l2Tx <- L2Tx.parse(tx.toCbor, ledgerConfig)
            next <- HydrozoaTransactionMutator.transit(ledgerConfig, time, state, l2Tx)
        } yield (l2Tx, next)

    private def outputId(l2Tx: L2Tx, index: Int): TransactionInput =
        TransactionInput(l2Tx.tx.id, index)

    test("mint: declared bundle lands in the overlay, main gains the projected output") {
        val utxo = mkPeerUtxo(1, Value.ada(100))
        val state = Compartments(Map(utxo), TransientTokens.empty)
        val tx = buildSignedL2Tx(
          spends = List(utxo),
          sends = List(sendToPeer(Value(Coin.ada(100), demoBundle(5)))),
          mints = List(mintDemo(5)),
          transientOutputs = Map(0 -> demoBundle(5))
        )

        val result = applyTransit(state, tx)
        val _ = assert(result.isRight, s"expected success, got: $result")
        val (l2Tx, next) = result.toOption.get
        val newId = outputId(l2Tx, 0)
        assert(next.transientTokens == Map(newId -> demoBundle(5)))
        assert(next.main.keySet == Set(newId))
        assert(next.main(newId).value == Value.ada(100), "main must hold the projected value")
    }

    test("burn: spending an overlaid utxo with a negative mint drains the overlay") {
        val (input, mainOutput) = mkPeerUtxo(2, Value.ada(100))
        val state = Compartments(Map(input -> mainOutput), Map(input -> demoBundle(5)))
        val combined = mainOutput.withValue(Value(Coin.ada(100), demoBundle(5)))
        val tx = buildSignedL2Tx(
          spends = List(input -> combined),
          sends = List(sendToPeer(Value.ada(100))),
          mints = List(mintDemo(-5))
        )

        val result = applyTransit(state, tx)
        val _ = assert(result.isRight, s"expected success, got: $result")
        val (l2Tx, next) = result.toOption.get
        assert(next.transientTokens.isEmpty)
        assert(next.main.keySet == Set(outputId(l2Tx, 0)))
        assert(next.main(outputId(l2Tx, 0)).value == Value.ada(100))
    }

    test("pass-through: re-declared bundle moves to the new utxo id without a mint field") {
        val (input, mainOutput) = mkPeerUtxo(3, Value.ada(100))
        val state = Compartments(Map(input -> mainOutput), Map(input -> demoBundle(5)))
        val combined = mainOutput.withValue(Value(Coin.ada(100), demoBundle(5)))
        val tx = buildSignedL2Tx(
          spends = List(input -> combined),
          sends = List(sendToPeer(Value(Coin.ada(100), demoBundle(5)))),
          transientOutputs = Map(0 -> demoBundle(5))
        )

        val result = applyTransit(state, tx)
        val _ = assert(result.isRight, s"expected success, got: $result")
        val (l2Tx, next) = result.toOption.get
        assert(next.transientTokens == Map(outputId(l2Tx, 0) -> demoBundle(5)))
        assert(next.main(outputId(l2Tx, 0)).value == Value.ada(100))
    }

    test("same policy id may live in both compartments simultaneously") {
        // 3 DEMO are L1-native (already in main); 5 more are minted transiently under the SAME
        // policy id. Only the declaration tells them apart.
        val utxo = mkPeerUtxo(4, Value(Coin.ada(100), demoBundle(3)))
        val state = Compartments(Map(utxo), TransientTokens.empty)
        val tx = buildSignedL2Tx(
          spends = List(utxo),
          sends = List(sendToPeer(Value(Coin.ada(100), demoBundle(8)))),
          mints = List(mintDemo(5)),
          transientOutputs = Map(0 -> demoBundle(5))
        )

        val result = applyTransit(state, tx)
        val _ = assert(result.isRight, s"expected success, got: $result")
        val (l2Tx, next) = result.toOption.get
        val newId = outputId(l2Tx, 0)
        assert(next.transientTokens == Map(newId -> demoBundle(5)))
        assert(
          next.main(newId).value == Value(Coin.ada(100), demoBundle(3)),
          "the L1-native 3 DEMO stay in main"
        )
    }

    test("minting into the main compartment fails by projection arithmetic") {
        // Minted tokens NOT declared transient: the projection keeps them in the outputs but
        // strips the mint field, so main-compartment conservation fails.
        val utxo = mkPeerUtxo(5, Value.ada(100))
        val state = Compartments(Map(utxo), TransientTokens.empty)
        val tx = buildSignedL2Tx(
          spends = List(utxo),
          sends = List(sendToPeer(Value(Coin.ada(100), demoBundle(5)))),
          mints = List(mintDemo(5))
        )

        val result = applyTransit(state, tx)
        assert(
          result.left.exists(_.toString.contains("L1-projection conservation")),
          s"expected an L1-projection conservation failure, got: $result"
        )
    }

    test("burning a main-compartment token fails by projection arithmetic") {
        // The 5 DEMO are L1-native (in main). Burning them balances the full tx, but the
        // projection strips the mint field and the tokens have nowhere to go.
        val utxo = mkPeerUtxo(6, Value(Coin.ada(100), demoBundle(5)))
        val state = Compartments(Map(utxo), TransientTokens.empty)
        val tx = buildSignedL2Tx(
          spends = List(utxo),
          sends = List(sendToPeer(Value.ada(100))),
          mints = List(mintDemo(-5))
        )

        val result = applyTransit(state, tx)
        assert(
          result.left.exists(_.toString.contains("L1-projection conservation")),
          s"expected an L1-projection conservation failure, got: $result"
        )
    }

    test("smuggling overlay tokens into main (spend overlaid, no declaration) fails") {
        val (input, mainOutput) = mkPeerUtxo(7, Value.ada(100))
        val state = Compartments(Map(input -> mainOutput), Map(input -> demoBundle(5)))
        val combined = mainOutput.withValue(Value(Coin.ada(100), demoBundle(5)))
        val tx = buildSignedL2Tx(
          spends = List(input -> combined),
          sends = List(sendToPeer(Value(Coin.ada(100), demoBundle(5))))
        )

        val result = applyTransit(state, tx)
        assert(
          result.left.exists(_.toString.contains("L1-projection conservation")),
          s"expected an L1-projection conservation failure, got: $result"
        )
    }

    test("declared bundle exceeding the output's assets is rejected at parse") {
        val utxo = mkPeerUtxo(8, Value.ada(100))
        val tx = buildSignedL2Tx(
          spends = List(utxo),
          sends = List(sendToPeer(Value(Coin.ada(100), demoBundle(5)))),
          mints = List(mintDemo(5)),
          transientOutputs = Map(0 -> demoBundle(7))
        )

        val result = L2Tx.parse(tx.toCbor, ledgerConfig)
        assert(
          result.left.exists(_.contains("exceeds the output's assets")),
          s"expected a sub-value rejection, got: $result"
        )
    }

    test("an L1-bound output with a transient declaration is rejected at parse") {
        val utxo = mkPeerUtxo(9, Value.ada(100))
        val tx = buildSignedL2Tx(
          spends = List(utxo),
          sends = List(sendToPeer(Value(Coin.ada(100), demoBundle(5)), marker = 1)),
          mints = List(mintDemo(5)),
          transientOutputs = Map(0 -> demoBundle(5))
        )

        val result = L2Tx.parse(tx.toCbor, ledgerConfig)
        assert(
          result.left.exists(_.contains("cannot carry transient tokens")),
          s"expected an L1-bound rejection, got: $result"
        )
    }

    test("legacy bare-list metadata still parses (no transient outputs)") {
        val utxo = mkPeerUtxo(10, Value.ada(100))
        val state = Compartments(Map(utxo), TransientTokens.empty)
        val tx = buildSignedL2Tx(
          spends = List(utxo),
          sends = List(sendToPeer(Value.ada(100))),
          legacyMetadataShape = true
        )

        val result = applyTransit(state, tx)
        val _ = assert(result.isRight, s"expected success, got: $result")
        val (l2Tx, next) = result.toOption.get
        assert(l2Tx.transientOutputs.isEmpty)
        assert(next.transientTokens.isEmpty)
    }
}
