package hydrozoa.rulebased.evacuator

import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.addrKeyHash
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.ShelleyPaymentPart
import scalus.cardano.ledger.Coin

/** Checks the map we intend to submit for real before we submit it.
  *
  * A synthetic map is only useful if it is indistinguishable from a head's own as far as the
  * validator is concerned — same key width, same payout shape, same commitment computation. These
  * are the properties that decide whether a run against it tells us anything, plus the funding
  * arithmetic that decides whether we can afford it.
  */
class SyntheticMapTest extends AnyFunSuite {

    private val env =
        MultiNodeConfig.generateWithCoil().pureApply(Gen.Parameters.default, Seed(0L))

    private given hydrozoa.config.head.network.CardanoNetwork.Section = env.headConfig

    private val payTo: ShelleyPaymentPart =
        ShelleyPaymentPart.Key(
          env.nodePrivateConfigs.head._2.ownWallet.exportVerificationKey.addrKeyHash
        )

    private val perEntry = Coin.ada(2)

    private def build(n: Int) =
        SyntheticMap(n, payTo, env.headConfig.network, perEntry)
            .fold(v => fail(s"map did not build: $v"), identity)

    test("a map has the requested number of distinct entries") {
        val map = build(500)
        val _ = assert(map.size == 500)
        assert(map.evacuationMap.keySet.size == 500, "keys collided")
    }

    test("entries clear the min-ada floor, so every payout is a valid output") {
        // Below the floor `Payout.Obligation` refuses to build at all — which is the check we want,
        // since an under-funded output would be rejected by the ledger, not by us.
        val tooSmall = SyntheticMap(10, payTo, env.headConfig.network, Coin(100_000L))
        val _ = assert(tooSmall.isLeft, "an output below min-ada was accepted as a payout")
        assert(build(10).size == 10)
    }

    test("the funding requirement is the sum of the payouts") {
        val map = build(500)
        assert(SyntheticMap.fundingRequired(map) == Coin(perEntry.value * 500))
    }

    test("a 500-entry map is affordable and plans to a chain worth testing") {
        val map = build(500)
        val locked = SyntheticMap.fundingRequired(map).value / 1_000_000L
        val txs = EvacuationPlan.txCount(map, env.headConfig.cardanoProtocolParams)

        // Sized against the funding wallet: deep enough to exercise chaining and backpressure,
        // cheap enough that the ada is a rounding error against what the wallet holds.
        val _ = assert(locked <= 2_000L, s"$locked ada is more lock-up than the test needs")
        assert(txs >= 30, s"$txs transactions is too short a chain to test chaining")
    }

    test("the map is reproducible, so a failing run can be replayed") {
        assert(build(50).kzgCommitment == build(50).kzgCommitment)
    }

    test("a prefix of a larger map is the smaller map") {
        // Growing the test map must not renumber the entries already used, or a run at 500 cannot
        // be compared against a run at 50.
        val small = build(50)
        val large = build(500)
        assert(large.evacuationMap.take(50) == small.evacuationMap)
    }
}
