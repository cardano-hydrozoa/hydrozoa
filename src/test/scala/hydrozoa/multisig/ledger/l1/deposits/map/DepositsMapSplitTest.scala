package hydrozoa.multisig.ledger.l1.deposits.map

import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.DepositAbsorptionStartTime
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l1.tx.genDepositUtxo
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import org.scalacheck.Prop.propBoolean
import org.scalacheck.rng.Seed
import org.scalacheck.{Gen, Prop, Properties}
import scala.collection.immutable.Queue
import scalus.cardano.ledger.Value

/** `DepositsMap.Partition.split` — the absorption cap.
  *
  * The cap counts '''deposits''', because that is what the settlement tx builder checks it against
  * (`SettlementTx`: `depositsToSpend.length <= maxDepositsAbsorbedPerBlock`). The map is two-layer,
  * one queue per absorption start time, and that time is slot-quantized — so deposits maturing in
  * the same second share a key, and a split that takes the first `n` '''keys''' takes more than `n`
  * deposits. That is the production failure these tests pin: *"Too many deposits were included. You
  * passed 133, but we can have at most 100"*, raised in `tryCloseAsLeader`, which wedges the head
  * at that block on every restart.
  *
  * The bug was invisible under light load: with one deposit per second, keys and deposits are the
  * same number.
  */
object DepositsMapSplitTest extends Properties("DepositsMap.split") {

    private val config: NodeConfig =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, Seed(0L))

    /** `k` deposit utxos with pairwise distinct absorption start times, ascending.
      *
      * Split cares only about an entry's identity and its slot, so the fixtures below reuse one
      * utxo per slot under fresh request ids rather than generating a utxo per deposit — which also
      * keeps a 250-deposit slot cheap to build.
      */
    private def slotBases(k: Int): List[DepositUtxo] = {
        val one =
            genDepositUtxo(config, Some(config.headMultisigAddress), Gen.const(Value.ada(10)))()
        val candidates =
            Gen.listOfN(k * 4, one).pureApply(Gen.Parameters.default, Seed(1L))
        val distinct = candidates
            .distinctBy(_.absorptionStartTime.instant)
            .sortBy(_.absorptionStartTime.instant)
        require(
          distinct.length >= k,
          s"fixture needs $k distinct absorption start times, generator produced ${distinct.length}"
        )
        distinct.take(k)
    }

    /** An eligible map whose i-th slot holds `sizes(i)` deposits. */
    private def eligibleWith(sizes: List[Int]): DepositsMap =
        sizes.zip(slotBases(sizes.length)).zipWithIndex.foldLeft(DepositsMap.empty) {
            case (m, ((count, base), slot)) =>
                val entries = Queue.from(
                  (0 until count)
                      .map(i => DepositsMap.Entry(RequestId(0, (slot * 10_000 + i).toLong), base))
                )
                m.append((base.absorptionStartTime: DepositAbsorptionStartTime, entries))
        }

    private def splitOf(sizes: List[Int], n: Int): DepositsMap.Split =
        DepositsMap
            .Partition(
              expired = DepositsMap.empty,
              eligible = eligibleWith(sizes),
              immature = DepositsMap.empty,
              notInPollResults = DepositsMap.empty
            )
            .split(n)

    // ----- regressions -------------------------------------------------------------------

    /** The reported failure, to the number. 33 slots of two deposits and 67 of one: 100 slots, 133
      * deposits. The negative control is the second conjunct — the old key-based split really does
      * hand 133 deposits to a builder that accepts at most 100.
      */
    val _ = property("100 slots holding 133 deposits absorb exactly 100 (bug repro)") = {
        val sizes = List.fill(33)(2) ++ List.fill(67)(1)
        val eligible = eligibleWith(sizes)
        val absorbed = splitOf(sizes, 100).absorbed.requestIds.length
        val byKeyAsBefore = DepositsMap(eligible.treeMap.splitAt(100)._1).requestIds.length
        (eligible.requestIds.length == 133) :| "fixture holds 133 deposits" &&
        (absorbed == 100) :| s"absorbed $absorbed, expected 100" &&
        (byKeyAsBefore == 133) :| "negative control: the key-based split overruns to 133"
    }

    /** A single second can hold more deposits than the whole cap, so no key-boundary split can
      * express the bound — the cut has to fall inside the queue.
      */
    val _ = property("one slot larger than the cap is split inside its queue") = {
        val s = splitOf(List(250), 100)
        (s.absorbed.requestIds.length == 100) :| "absorbed 100" &&
        (s.unabsorbed.requestIds.length == 150) :| "unabsorbed 150" &&
        (s.absorbed.treeMap.keySet == s.unabsorbed.treeMap.keySet) :|
            "the boundary slot appears in both halves"
    }

    /** The budget running out exactly on a slot boundary leaves that slot wholly unabsorbed — the
      * empty-head case of the cut.
      */
    val _ = property("a budget landing on a slot boundary absorbs no part of the next slot") = {
        val s = splitOf(List(60, 40, 25), 100)
        (s.absorbed.requestIds.length == 100) :| "absorbed 100" &&
        (s.absorbed.treeMap.size == 2) :| "only the two full slots" &&
        (s.unabsorbed.requestIds.length == 25) :| "the third slot survives whole"
    }

    val _ = property("everything below the cap is absorbed") = {
        val s = splitOf(List(3, 4), 100)
        (s.absorbed.requestIds.length == 7) :| "all seven absorbed" &&
        s.unabsorbed.requestIds.isEmpty :| "nothing left over"
    }

    // ----- properties --------------------------------------------------------------------

    private val genSizes: Gen[List[Int]] =
        Gen.choose(1, 12).flatMap(k => Gen.listOfN(k, Gen.choose(1, 20)))

    val _ = property("absorbs exactly the budget, or everything if there is less") =
        Prop.forAll(genSizes, Gen.choose(1, 60)) { (sizes, n) =>
            val s = splitOf(sizes, n)
            s.absorbed.requestIds.length == math.min(n, eligibleWith(sizes).requestIds.length)
        }

    /** The semantics the cap rides on: splitting only decides *where* the eligible stream is cut,
      * never reorders or drops any part of it.
      */
    val _ = property("absorbed then unabsorbed reconstructs the eligible stream in order") =
        Prop.forAll(genSizes, Gen.choose(1, 60)) { (sizes, n) =>
            val s = splitOf(sizes, n)
            s.absorbed.requestIds ++ s.unabsorbed.requestIds == eligibleWith(sizes).requestIds
        }

    val _ = property("every absorbed slot is at or before every unabsorbed slot") =
        Prop.forAll(genSizes, Gen.choose(1, 60)) { (sizes, n) =>
            val s = splitOf(sizes, n)
            val lastAbsorbed = s.absorbed.treeMap.keys.maxOption.map(_.instant)
            val firstUnabsorbed = s.unabsorbed.treeMap.keys.minOption.map(_.instant)
            (lastAbsorbed, firstUnabsorbed) match {
                case (Some(a), Some(b)) => !a.isAfter(b)
                case _                  => true
            }
        }
}
