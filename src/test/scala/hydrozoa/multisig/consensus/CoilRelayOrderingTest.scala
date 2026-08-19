package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.multisig.consensus.liaison.{LaneOutbound, LiaisonProtocol}
import hydrozoa.multisig.ledger.block.{BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockVersion}
import org.scalacheck.{Prop, Properties}
import scala.concurrent.duration.DurationInt
import test.{SeedPhrase, TestPeers}

/** Finding #38 — the hub→coil **block lane** is fed by two different actors, so its gap-free
  * contiguous numbering rests on the relative scheduling of two fibers rather than on anything that
  * enforces order.
  *
  * On head peer `h` with `nHeadPeers = 2`, leadership alternates every block
  * ([[hydrozoa.multisig.consensus.peer.HeadPeerId.isLeader]] is
  * `blockNum % nHeadPeers == peerNum`), so consecutive briefs always arrive at [[CoilRelay]] from
  * *different* feeders:
  *   - remote-led `N` — `PeerLiaisonHeadToHead.dispatch`, which forwards to the relay **last**,
  *     after it has already handed the brief and the remote's soft-ack to consensus;
  *   - own-led `N+1` — `JointLedger`, reached by exactly those two forwards.
  *
  * So everything needed to produce `N+1` is set in motion before `N` is forwarded, and if the
  * dispatch fiber loses its worker in between, `N+1` is enqueued first. That is what killed head-0
  * on the t3 at block 4922 (`last=Some(4920) attempted=4922 expected=Some(4921)`).
  *
  * Three properties pin the behaviour: with two feeders the own-led brief can overtake the
  * remote-led one and raise the exact crash (`ownLedOvertakingRemoteLedRaisesTheT3Error`); the
  * undisturbed interleaving is fine (`meshFirstIsFine`, which is why it survived 4921 blocks); and
  * a single feeder is order-independent by construction (`singleFeederIsOrderIndependent`) — the
  * shape of the fix now wired in [[JointLedger]] (blocks) and [[StackComposer]] (stacks).
  */
object CoilRelayOrderingTest extends Properties("CoilRelay block-lane ordering") {

    private val network = CardanoNetwork.Preprod

    private val headConfig = {
        val headPeers = TestPeers.apply(SeedPhrase.Yaci, network, 2)
        MultiNodeConfig.generateWith(headPeers)().sample.get.headConfig
    }

    private def brief(blockNum: Int, now: QuantizedInstant): BlockBrief.Next = {
        val end = BlockCreationEndTime(now + 1.second)
        val fallback = headConfig.txTiming.newFallbackStartTime(end)
        BlockBrief.Minor(
          BlockHeader.Minor(
            blockNum = BlockNumber(blockNum),
            blockVersion = BlockVersion.Full(0, 0),
            startTime = BlockCreationStartTime(now),
            endTime = end,
            fallbackTxStartTime = fallback,
            forcedMajorBlockWakeupTime = headConfig.txTiming.forcedMajorBlockWakeupTime(fallback),
            mDepositDecisionWakeupTime = None
          ),
          BlockBody.Minor(requests = List.empty, depositsRejected = List.empty)
        )
    }

    /** The real hub→coil block lane, built exactly as `PeerLiaisonHubToCoil` builds it (`:93`).
      * `backfill` is unused here — nothing in this scenario reads below the outbox floor.
      */
    private def blockLane: LaneOutbound[BlockBrief.Next, BlockNumber] =
        LaneOutbound.contiguous[BlockBrief.Next, BlockNumber](
          _.blockNum,
          BlockNumber(1),
          _.increment,
          backfill = (_, _) => IO.pure(Nil)
        )

    /** Stands in for `PeerLiaisonHubToCoil`, doing what its `appendArtifact` does for a block brief
      * (`:244`) and nothing else.
      */
    private class LiaisonStub(
        lane: LaneOutbound[BlockBrief.Next, BlockNumber],
        raised: cats.effect.Ref[IO, Vector[Throwable]],
        appended: cats.effect.Ref[IO, Int]
    ) extends Actor[IO, LiaisonProtocol.HubToCoilRequest] {
        override def receive: Receive[IO, LiaisonProtocol.HubToCoilRequest] = {
            // Record and re-raise: the raise is the incident (it kills the actor system), and the
            // record is how the harness observes it afterwards. `appended` counts briefs the lane
            // has finished with either way, which is what [[run]] waits on.
            case b: BlockBrief.Next =>
                lane.append(b).onError(t => raised.update(_ :+ t)).guarantee(appended.update(_ + 1))
            case _ => IO.unit
        }
    }

    /** Which fiber reaches [[CoilRelay]] first with its brief. The remote-led brief `N` is
      * forwarded by `PeerLiaisonHeadToHead.dispatch` as its *last* act; the own-led brief `N+1` is
      * produced by `JointLedger`, which dispatch already unblocked two steps earlier. Nothing
      * orders the two.
      */
    private enum Interleaving:
        /** dispatch keeps its worker through the steps-5→7 window — the usual case. */
        case MeshFirst

        /** dispatch is descheduled in that window and the own-led brief overtakes it — the t3. */
        case OwnLedFirst

        /** The fix: one actor owns the lane, so it emits in spine order whoever is descheduled. */
        case SingleFeeder

    /** Wait for the scenario to settle: both briefs through the lane, or one of them raised — which
      * takes the actor system down, so the other never lands. The sends are asynchronous, so
      * something has to gate the read below; a fixed sleep expires mid-drain on a loaded box and
      * the read comes back short, which is a false falsification (finding #63). The bound turns a
      * genuine stall back into a property failure rather than a hang.
      */
    private def awaitSettled(
        appended: cats.effect.Ref[IO, Int],
        raised: cats.effect.Ref[IO, Vector[Throwable]]
    ): IO[Unit] =
        (appended.get, raised.get)
            .mapN((n, errs) => n >= 2 || errs.nonEmpty)
            .flatTap(settled => if settled then IO.unit else IO.sleep(2.millis))
            .iterateUntil(identity)
            .void
            .timeoutTo(10.seconds, IO.unit)

    /** Drive a real [[CoilRelay]] and a real block lane with the given interleaving, and return
      * whatever the lane raised. The lane resumes at the high-water the crash reports and the
      * briefs are the ones it reports, so a reproduction is recognisable by its message.
      */
    private def run(interleaving: Interleaving): (Option[Throwable], List[Int]) = {
        val io = for {
            raised <- cats.effect.Ref[IO].of(Vector.empty[Throwable])
            appended <- cats.effect.Ref[IO].of(0)
            delivered <- cats.effect.Ref[IO].of(List.empty[Int])
            _ <- ActorSystem[IO]("coil-relay-ordering-test").use { system =>
                for {
                    now <- realTimeQuantizedInstant(headConfig.slotConfig)
                    lane = blockLane
                    _ <- lane.seedHighWater(Some(BlockNumber(4920)))
                    liaison <- system.actorOf(new LiaisonStub(lane, raised, appended))
                    relay <- system.actorOf(
                      CoilRelay(CoilRelay.Connections(coilPeerLiaisons = List(liaison)))
                    )
                    _ <- IO.sleep(50.millis) // let CoilRelay.PreStart resolve its connections
                    remoteLed = relay ! brief(4921, now)
                    ownLed = relay ! brief(4922, now)
                    // The sleeps below only space the two sends; which brief the relay sees first
                    // is fixed by the send order and its FIFO mailbox, not by the clock.
                    _ <- interleaving match {
                        case Interleaving.MeshFirst =>
                            remoteLed >> IO.sleep(50.millis) >> ownLed
                        case Interleaving.OwnLedFirst =>
                            ownLed >> IO.sleep(50.millis) >> remoteLed
                        // One feeder, so its own sends are ordered by `!` and the lane cannot
                        // see a gap no matter which fiber was descheduled.
                        case Interleaving.SingleFeeder =>
                            remoteLed >> ownLed
                    }
                    _ <- awaitSettled(appended, raised)
                    // Presence-of-effect: the briefs the lane will actually serve a coil
                    // peer, in order. The block lane's `maxPerReply` is 1, so pull twice.
                    served <- List(4921, 4922).traverse(n =>
                        lane.reply(BlockNumber(n)).attempt.map {
                            case Right(LaneOutbound.Items(items)) =>
                                items.map(_.blockNum.convert.toInt)
                            case _ => Nil
                        }
                    )
                    _ <- delivered.set(served.flatten)
                } yield ()
            }.attempt // the raise takes the actor system down; that is the incident
            errs <- raised.get
            n <- delivered.get
        } yield (errs.headOption, n)
        io.unsafeRunSync()
    }

    private def isTheCrash(t: Throwable): Boolean = t match {
        case LaneOutbound.AppendOutOfOrder("Some(4920)", "4922", "Some(4921)") => true
        case _                                                                 => false
    }

    /** The t3: the own-led brief overtakes the remote-led one and the lane raises the exact error
      * head-0 died on.
      */
    val _ = property("ownLedOvertakingRemoteLedRaisesTheT3Error") = Prop {
        val (raised, delivered) = run(Interleaving.OwnLedFirst)
        raised.exists(isTheCrash) && delivered != List(4921, 4922)
    }

    /** The same two feeders, same code, undisturbed — which is why it survived 4921 blocks. */
    val _ = property("meshFirstIsFine") = Prop {
        val (raised, delivered) = run(Interleaving.MeshFirst)
        raised.isEmpty && delivered == List(4921, 4922)
    }

    /** The fix: a single feeder is order-independent by construction. */
    val _ = property("singleFeederIsOrderIndependent") = Prop {
        val (raised, delivered) = run(Interleaving.SingleFeeder)
        raised.isEmpty && delivered == List(4921, 4922)
    }
}
