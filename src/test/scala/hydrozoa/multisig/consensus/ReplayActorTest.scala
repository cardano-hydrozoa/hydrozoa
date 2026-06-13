package hydrozoa.multisig.consensus

import cats.data.NonEmptyList
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.multisig.timing.TxTiming.StackTimes.StackCreationEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState}
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.ledger.block.{BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.{PartitionEffects, Stack, StackBrief, StackEffects, StackNumber, StandaloneEvacuationCommitment}
import hydrozoa.multisig.persistence.{ArrivalStamp, Cf, InMemoryBackendStore, LaneKey, LaneValue, Persistence, StoreKey}
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt
import scalus.uplc.builtin.ByteString

/** Records every message routed to it (a stand-in for a consensus actor). */
private final case class Recorder[R](sink: Ref[IO, Vector[R]]) extends Actor[IO, R]:
    override def receive: Receive[IO, R] = { case m => sink.update(_ :+ m).void }

/** R3b/R3c test for `ReplayActor.replay`: it reads a seeded store + a one-shot L1 sample and routes
  * the recovered tail into the consensus actors' mailboxes (and refuses to start on an inconsistent
  * store). Probe `Recorder`s stand in for the four targets so we can assert the routing directly.
  */
class ReplayActorTest extends AnyFunSuite:

    private val config: NodeConfig =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(0L))
    private val headConfig: HeadConfig = config.headConfig
    private given CardanoNetwork.Section = config
    private val stamp: ArrivalStamp = ArrivalStamp(generation = 0, monotonicNanos = 1L)

    /** This node's head peer number (the fixture config is always a head node). */
    private val ownNum: HeadPeerNumber = config.ownPeerId match {
        case PeerId.Head(n) => n
        case PeerId.Coil(_) => fail("fixture config must be a head node")
    }

    test(
      "ReplayActor routes the recovered tail into the right mailboxes (floors + in-flight stack)"
    ) {
        val c = runReplay(seedRecoverable)
        assert(c.outcome.isRight, s"replay failed: ${c.outcome}")
        // BlockWeaver: first PollResults + the block brief (>= softAcked+1 = 0).
        assert(c.bw.exists(_.isInstanceOf[PollResults]), "BW PollResults")
        assert(hasBlock(c.bw, 5), "BW block 5")
        // FastConsensusActor: the block brief (aggregator floor softConfirmed+1).
        assert(hasBlock(c.fca, 5), "FCA block 5")
        // SlowConsensusActor: the reconstructed in-flight handoff + every HardAck (own round-1 +
        // round-2 for stack 1, plus the remote peer's).
        assert(c.sca.exists(_.isInstanceOf[SlowConsensusActor.StackHandoff]), "SCA handoff")
        assert(c.sca.count(_.isInstanceOf[HardAck]) == 3, "SCA 3 hard-acks")
        // StackComposer: only stack >= hardAcked+1 = 2 (stack 1 is the in-flight band, handled by
        // the handoff, not its brief).
        assert(
          c.sc.collect { case b: StackBrief => b.stackNum } == Vector(StackNumber(2)),
          "SC only stack 2"
        )
    }

    test("ReplayActor refuses to start on an inconsistent store (confirmed > acked)") {
        val c = runReplay(seedInconsistent)
        assert(
          c.outcome.swap.toOption.exists(_.isInstanceOf[IllegalStateException]),
          s"expected IllegalStateException, got ${c.outcome}"
        )
    }

    // ---- harness ----

    private final case class Captured(
        bw: Vector[BlockWeaver.Request],
        fca: Vector[FastConsensusActor.Request],
        sca: Vector[SlowConsensusActor.Request],
        sc: Vector[StackComposer.Request],
        outcome: Either[Throwable, Unit]
    )

    /** Boot four probe targets + a mock L1, seed the store, run `replay`, settle, and capture both
      * each probe's received messages and the replay outcome.
      */
    private def runReplay(seed: Persistence[IO] => IO[Unit]): Captured =
        val own = ownNum
        val peers = config.headPeerIds.map(_.peerNum).toList
        val treasuryAddress = config.initializationTx.treasuryProduced.address
        InMemoryBackendStore.open
            .use(backend =>
                ActorSystem[IO]("replay-test").use(system =>
                    for {
                        persistence <- Persistence.fromBackend(backend)
                        cardanoBackend <- CardanoBackendMock.mockIO(MockState(Map.empty))
                        bwSink <- Ref.of[IO, Vector[BlockWeaver.Request]](Vector.empty)
                        fcaSink <- Ref.of[IO, Vector[FastConsensusActor.Request]](Vector.empty)
                        scaSink <- Ref.of[IO, Vector[SlowConsensusActor.Request]](Vector.empty)
                        scSink <- Ref.of[IO, Vector[StackComposer.Request]](Vector.empty)
                        bw <- system.actorOf(Recorder[BlockWeaver.Request](bwSink))
                        fca <- system.actorOf(Recorder[FastConsensusActor.Request](fcaSink))
                        sca <- system.actorOf(Recorder[SlowConsensusActor.Request](scaSink))
                        sc <- system.actorOf(Recorder[StackComposer.Request](scSink))
                        _ <- seed(persistence)
                        outcome <- ReplayActor
                            .replay(
                              persistence,
                              cardanoBackend,
                              ReplayActor.Targets(bw, fca, sca, sc),
                              own,
                              peers,
                              treasuryAddress
                            )
                            .attempt
                        _ <- IO.sleep(1.second) // let the probe fibers drain their mailboxes
                        bwMsgs <- bwSink.get
                        fcaMsgs <- fcaSink.get
                        scaMsgs <- scaSink.get
                        scMsgs <- scSink.get
                    } yield Captured(bwMsgs, fcaMsgs, scaMsgs, scMsgs, outcome)
                )
            )
            .unsafeRunSync()

    private def hasBlock(msgs: Vector[?], blockNum: Int): Boolean =
        msgs.exists {
            case b: BlockBrief.Next => b.blockNum == BlockNumber(blockNum)
            case _                  => false
        }

    /** Stack 0 hard-confirmed (`hardConfirmed = 0`), stack 1 acked but not confirmed (the in-flight
      * band: own round-1 + round-2 + its `UnsignedStack`, plus a remote ack), a block brief, and
      * two stack briefs (1 + 2) to exercise the composer floor.
      */
    private def seedRecoverable(p: Persistence[IO]): IO[Unit] =
        val own = ownNum
        val other = HeadPeerNumber(1)
        for {
            // hardConfirmed = Some(0): the marker reads only the HardConfirmation KEY, so a dummy
            // value byte suffices (the typed value's leaf txs have no public ctors).
            _ <- p.backend.put(
              Cf.HardConfirmation,
              StoreKey.HardConfirmation(StackNumber(0)).encode,
              Array[Byte](0)
            )
            // Own hard-acks for stack 1 → hardAcked = 1, hardAckedStack = 1 (> hardConfirmed 0).
            _ <- p.put(LaneKey.HardAck(own, HardAckNumber(0)))(
              LaneValue(stamp, hardAck(own.convert, 0, 1))
            )
            _ <- p.put(LaneKey.HardAck(own, HardAckNumber(1)))(
              LaneValue(stamp, hardAck(own.convert, 1, 1))
            )
            // A remote peer's hard-ack for stack 1 → routed to SCA via the tail.
            _ <- p.put(LaneKey.HardAck(other, HardAckNumber(0)))(
              LaneValue(stamp, hardAck(other.convert, 0, 1))
            )
            // The in-flight stack's unsigned form → reconstructed handoff to SCA.
            unsigned <- unsignedStack(1)
            _ <- p.put(StoreKey.UnsignedStack(StackNumber(1)))(unsigned)
            // A block brief → FCA + BlockWeaver.
            block5 <- blockBrief(5)
            _ <- p.put(LaneKey.Block(BlockNumber(5)))(LaneValue(stamp, block5))
            // Two stack briefs → SC gets only stack 2 (>= composer floor 2).
            s1 <- stackBrief(1, 0, 0)
            s2 <- stackBrief(2, 1, 1)
            _ <- p.put(LaneKey.Stack(StackNumber(1)))(LaneValue(stamp, s1))
            _ <- p.put(LaneKey.Stack(StackNumber(2)))(LaneValue(stamp, s2))
        } yield ()

    /** softConfirmed = 5 but softAcked = 2 → confirmed > acked, a torn store. Markers read only the
      * keys, so dummy value bytes suffice.
      */
    private def seedInconsistent(p: Persistence[IO]): IO[Unit] =
        val own = ownNum
        for {
            _ <- p.backend.put(
              Cf.SoftConfirmation,
              StoreKey.SoftConfirmation(BlockNumber(5)).encode,
              Array[Byte](0)
            )
            _ <- p.backend.put(
              Cf.SoftAck(own),
              LaneKey.SoftAck(own, SoftAckNumber(2)).encode,
              Array[Byte](0)
            )
        } yield ()

    // ---- fixtures (mirror RecoverSeamsTest) ----

    private def unsignedStack(stack: Int): IO[Stack.Unsigned] =
        stackBrief(stack, 0, 0).map { brief =>
            val sec = StandaloneEvacuationCommitment(
              blockNum = BlockNumber(0),
              blockVersion = BlockVersion.Full(0, 0),
              kzgCommitment = ByteString.fromArray(Array.fill[Byte](48)(0)),
              header = StandaloneEvacuationCommitment.Onchain.Serialized.fromBytes(
                Array.fill[Byte](32)(7)
              )
            )
            Stack.Unsigned(
              brief,
              StackEffects.Unsigned.Regular(
                NonEmptyList.one[PartitionEffects[StandaloneEvacuationCommitment]](
                  PartitionEffects.Minor(sec, List.empty)
                )
              )
            )
        }

    private def blockBrief(blockNum: Int): IO[BlockBrief.Next] =
        realTimeQuantizedInstant(headConfig.slotConfig).map { now =>
            val end = BlockCreationEndTime(now + 1.second)
            val fallback = headConfig.txTiming.newFallbackStartTime(end)
            BlockBrief.Minor(
              BlockHeader.Minor(
                blockNum = BlockNumber(blockNum),
                blockVersion = BlockVersion.Full(0, 0),
                startTime = BlockCreationStartTime(now),
                endTime = end,
                fallbackTxStartTime = fallback,
                forcedMajorBlockWakeupTime =
                    headConfig.txTiming.forcedMajorBlockWakeupTime(fallback),
                mDepositDecisionWakeupTime = None
              ),
              BlockBody.Minor(events = List.empty, depositsRefunded = List.empty)
            )
        }

    private def stackBrief(stack: Int, firstBlock: Int, lastBlock: Int): IO[StackBrief] =
        realTimeQuantizedInstant(headConfig.slotConfig).map { now =>
            StackBrief(
              stackNum = StackNumber(stack),
              firstBlockNum = BlockNumber(firstBlock),
              lastBlockNum = BlockNumber(lastBlock),
              creationEndTime = StackCreationEndTime(now)
            )
        }

    private def hardAck(peer: Int, ackNum: Int, stack: Int): HardAck =
        HardAck(
          ackId = HardAckId(PeerId.Head(HeadPeerNumber(peer)), HardAckNumber(ackNum)),
          stackNum = StackNumber(stack),
          payload = HardAck.Round2Payload.Regular(TxSignature(IArray.from(Array.fill[Byte](64)(0))))
        )
