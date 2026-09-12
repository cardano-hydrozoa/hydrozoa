package hydrozoa.multisig.ledger.eutxol2.store

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.eutxol2.tx.{GenesisObligation, L2Genesis}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l2.L2CommandNumber
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scala.collection.immutable.Queue
import scalus.cardano.ledger.{Blake2b_256, Coin, Hash, HashPurpose, MultiAsset, TransactionInput, TransactionOutput, Value}
import scalus.uplc.builtin.ByteString
import test.Generators.Hydrozoa.genGenesisObligation

/** [[L2Snapshot.stateHash]] — the built-in ledger's [[hydrozoa.multisig.ledger.l2.L2StateHash]]
  * construction (`docs/spec/l2-state-certificate.md`).
  *
  * The digest is what a head certifies on its settlement datums and SECs, and every peer derives
  * its own copy of an effect body before verifying the hard-ack signatures over it, so the two
  * properties that matter are that it depends on **exactly** the ledger state and that it depends
  * on **nothing else** — not on a `Map`'s iteration order and not on the coordination index.
  */
class L2SnapshotStateHashTest extends AnyFunSuite, ScalaCheckPropertyChecks:

    private val multiNodeConfig =
        MultiNodeConfig.generateDefault.pureApply(Gen.Parameters.default, Seed(0L))

    private given CardanoNetwork.Section = multiNodeConfig.nodeConfigs(HeadPeerNumber.zero)

    private val address = multiNodeConfig.addressOf(HeadPeerNumber.zero)

    private val genValue: Gen[Value] =
        Gen.choose(2_000_000L, 20_000_000L).map(n => Value(Coin(n)))

    private val genObligation: Gen[GenesisObligation] =
        genGenesisObligation(address, genValue = genValue)

    private val genOutput: Gen[TransactionOutput] = genObligation.map(_.toTransactionOutput)

    private val genInput: Gen[TransactionInput] =
        for {
            bytes <- Gen.listOfN(32, Gen.choose(0, 255).map(_.toByte))
            index <- Gen.choose(0, 4)
        } yield TransactionInput(
          transactionId = Hash[Blake2b_256, HashPurpose.TransactionHash](
            ByteString.fromArray(bytes.toArray)
          ),
          index = index
        )

    private val genGenesis: Gen[L2Genesis] =
        for {
            obligations <- Gen.listOfN(2, genObligation)
            id <- genInput
        } yield L2Genesis(Queue.from(obligations), L2Genesis.mkGenesisId(id))

    /** A snapshot with utxos and pending deposits, and an overlay over some subset of the utxos —
      * `Gen.someOf` can draw the empty subset, so a test that needs a non-empty overlay filters.
      */
    private val genSnapshot: Gen[L2Snapshot] =
        for {
            n <- Gen.choose(1, 6)
            inputs <- Gen.listOfN(n, genInput).map(_.distinct)
            outputs <- Gen.listOfN(inputs.size, genOutput)
            overlaid <- Gen.someOf(inputs)
            deposits <- Gen.listOfN(2, genGenesis)
            commandNumber <- Gen.choose(0L, 10_000L)
        } yield L2Snapshot(
          commandNumber = L2CommandNumber(commandNumber),
          activeUtxos = inputs.zip(outputs).toMap,
          transientTokens = overlaid.map(_ -> MultiAsset.empty).toMap,
          pendingDeposits = deposits.zipWithIndex.map((g, i) => RequestId(0, i.toLong) -> g).toMap
        )

    test("the digest is 32 bytes") {
        forAll(genSnapshot) { snapshot =>
            assert(snapshot.stateHash.byteString.size == 32)
        }
    }

    test("the digest does not depend on the order the maps were built in") {
        forAll(genSnapshot) { snapshot =>
            val reversed = snapshot.copy(
              activeUtxos = snapshot.activeUtxos.toList.reverse.toMap,
              transientTokens = snapshot.transientTokens.toList.reverse.toMap,
              pendingDeposits = snapshot.pendingDeposits.toList.reverse.toMap
            )
            assert(reversed.stateHash == snapshot.stateHash)
        }
    }

    test("the digest does not depend on the command number") {
        forAll(genSnapshot, Gen.choose(0L, 10_000L)) { (snapshot, other) =>
            val moved = snapshot.copy(commandNumber = L2CommandNumber(other))
            assert(moved.stateHash == snapshot.stateHash)
        }
    }

    test("dropping a utxo changes the digest") {
        forAll(genSnapshot) { snapshot =>
            val without =
                snapshot.copy(activeUtxos = snapshot.activeUtxos.tail)
            assert(without.stateHash != snapshot.stateHash)
        }
    }

    test("moving value between two utxos changes the digest") {
        forAll(genSnapshot.suchThat(_.activeUtxos.size >= 2)) { snapshot =>
            val (a, b) = (snapshot.activeUtxos.head, snapshot.activeUtxos.tail.head)
            val swapped = snapshot.copy(
              activeUtxos = snapshot.activeUtxos + (a._1 -> b._2) + (b._1 -> a._2)
            )
            // Identical outputs make the swap a no-op; only assert when it really moved something.
            whenever(a._2 != b._2) {
                assert(swapped.stateHash != snapshot.stateHash)
            }
        }
    }

    test("dropping a transient-token overlay entry changes the digest") {
        forAll(genSnapshot.suchThat(_.transientTokens.nonEmpty)) { snapshot =>
            val without = snapshot.copy(transientTokens = snapshot.transientTokens.tail)
            assert(without.stateHash != snapshot.stateHash)
        }
    }

    test("dropping a pending deposit changes the digest") {
        forAll(genSnapshot) { snapshot =>
            val without = snapshot.copy(pendingDeposits = snapshot.pendingDeposits.tail)
            assert(without.stateHash != snapshot.stateHash)
        }
    }

    test("an empty state hashes to a defined value, not an absence") {
        val empty = L2Snapshot(L2CommandNumber.zero, Map.empty, Map.empty, Map.empty)
        forAll(genSnapshot) { snapshot =>
            assert(
              empty.stateHash.byteString.size == 32
                  && snapshot.stateHash != empty.stateHash
            )
        }
    }
