package hydrozoa.multisig.ledger.l1.tx

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, FallbackTxStartTime, SettlementTxEndTime}
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l1.deposits.map.{DepositsMap, DepositsMapEvent}
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Gen, Prop, Properties}
import scalus.cardano.ledger.*
import test.*

/** The coil-follower deposit-existence oracle (`DepositsMap.Existence`).
  *
  * A coil peer below the settlement quorum can fall behind a settlement that has already been
  * submitted to L1, spending the absorbed deposits. Its fresh L1 poll would then report those
  * deposits gone, land them in `NotInPollResults`, and produce a brief that mismatches the leader's
  * — the consensus failure observed in production. `Existence.FromLeaderView` replays the head
  * peers' soft-confirmed verdict instead: a deposit is existent iff the leader did not reject it.
  */
object DepositsMapExistenceTest extends Properties("DepositsMap existence oracle") {

    private val tracer: ContraTracer[IO, DepositsMapEvent] =
        ContraTracer[IO, DepositsMapEvent](_ => IO.unit)

    /** Partition a single-entry map with the deposit deliberately mature and unexpired at the given
      * block/settlement times, so the outcome turns solely on the existence oracle.
      */
    private def classify(
        config: NodeConfig,
        entry: DepositsMap.Entry,
        existence: DepositsMap.Existence
    ): DepositsMap.Partition = {
        val du = entry.depositUtxo
        // bce == absorptionStartTime => not immature; ste = start - silence <= absorptionEndTime
        // => not expired. The deposit therefore reaches the existence check.
        val bce = BlockCreationEndTime(du.absorptionStartTime)
        val ste: SettlementTxEndTime =
            config.txTiming.newSettlementEndTime(FallbackTxStartTime(du.absorptionStartTime))
        DepositsMap.empty
            .append(entry)
            .partition(tracer)(bce, ste, existence)
            .unsafeRunSync()
    }

    private val genEnv: Gen[(NodeConfig, DepositsMap.Entry)] =
        for {
            mnc <- MultiNodeConfig.generate(TestPeersSpec.default)()
            config = mnc.nodeConfigs(HeadPeerNumber.zero)
            du <- genDepositUtxo(
              config,
              Some(config.headMultisigAddress),
              Gen.const(Value.ada(10))
            )()
        } yield (config, DepositsMap.Entry(RequestId(0, 1L), du))

    val _ = property(
      "fresh poll rejects a deposit a submitted settlement already spent (bug repro)"
    ) = Prop.forAll(genEnv) { case (config, entry) =>
        val p = classify(config, entry, DepositsMap.Existence.FromPoll(PollResults.empty))
        (p.notInPollResults.requestIds == List(entry.requestId)) :| "landed in NotInPollResults" &&
        p.eligible.requestIds.isEmpty :| "not eligible"
    }

    val _ = property("coil leader-view absorbs the same deposit despite the empty poll") =
        Prop.forAll(genEnv) { case (config, entry) =>
            val p = classify(config, entry, DepositsMap.Existence.FromLeaderView(Set.empty))
            (p.eligible.requestIds == List(entry.requestId)) :| "eligible" &&
            p.notInPollResults.requestIds.isEmpty :| "not rejected"
        }

    val _ = property(
      "coil leader-view replays the leader's rejection of a truly non-existent deposit"
    ) = Prop.forAll(genEnv) { case (config, entry) =>
        val p =
            classify(config, entry, DepositsMap.Existence.FromLeaderView(Set(entry.requestId)))
        (p.notInPollResults.requestIds == List(entry.requestId)) :| "rejected as non-existent" &&
        p.eligible.requestIds.isEmpty :| "not eligible"
    }
}
