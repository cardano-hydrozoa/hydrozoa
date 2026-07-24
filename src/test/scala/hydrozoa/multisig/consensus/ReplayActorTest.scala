package hydrozoa.multisig.consensus

import cats.data.NonEmptyList
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.multisig.timing.TxTiming.StackTimes.StackCreationEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState}
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber, HardAckWithId, HubHardAckNumber}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.ledger.block.{BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockResult, BlockVersion}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.{PartitionEffects, Stack, StackBrief, StackEffects, StackNumber, StandaloneEvacuationCommitment}
import hydrozoa.multisig.persistence.{ArrivalStamp, Cf, InMemoryBackendStore, JournalKey, JournalValue, Persistence, PersistenceEventFormat, StoreKey}
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
        val _ = assert(c.outcome.isRight, s"replay failed: ${c.outcome}")
        // BlockWeaver: first PollResults + the block brief (>= fastBlockMark+1 = 0).
        val _ = assert(c.bw.exists(_.isInstanceOf[PollResults]), "BW PollResults")
        val _ = assert(hasBlock(c.bw, 5), "BW block 5")
        // FastConsensusActor: the block brief (aggregator floor softConfirmed+1).
        val _ = assert(hasBlock(c.fca, 5), "FCA block 5")
        // SlowConsensusActor: the reconstructed in-flight handoff + every HardAck (own round-1 +
        // round-2 for stack 1, plus the remote peer's).
        val _ = assert(c.sca.exists(_.isInstanceOf[SlowConsensusActor.StackHandoff]), "SCA handoff")
        val _ = assert(c.sca.count(_.isInstanceOf[HardAck]) == 3, "SCA 3 hard-acks")
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

    test("ReplayActor (head) replays the hubs' HubHardAck to SCA (coil-quorum recovery, P14)") {
        val c = runReplay(seedHubHardAck, hubs = List(HeadPeerNumber(1)))
        val _ = assert(c.outcome.isRight, s"replay failed: ${c.outcome}")
        // The hub's HubHardAck, unwrapped to its `.ack`, reaches SlowConsensusActor.
        assert(c.sca.count(_.isInstanceOf[HardAck]) == 1, "SCA gets the hub-relayed ack")
    }

    test("ReplayActor (hub) re-feeds the unstamped coil-ack gap to CoilAckSequencer") {
        // The hub durably received coil 0's acks 0..2 (its coil HardAck receive copy) but only
        // stamped through 0 (CoilStampMark floor). Replay must re-feed the gap {1, 2} — above the
        // mark — and nothing at or below it.
        val coil = CoilPeerNumber(0)
        val c = runReplay(
          seed = p =>
              p.put(JournalKey.HardAck(PeerId.Coil(coil), HardAckNumber(0)))(
                JournalValue(stamp, coilHardAck(0, 0, stack = 1))
              ) >>
                  p.put(JournalKey.HardAck(PeerId.Coil(coil), HardAckNumber(1)))(
                    JournalValue(stamp, coilHardAck(0, 1, stack = 2))
                  ) >>
                  p.put(JournalKey.HardAck(PeerId.Coil(coil), HardAckNumber(2)))(
                    JournalValue(stamp, coilHardAck(0, 2, stack = 3))
                  ) >>
                  p.put(StoreKey.CoilStampMark)(Map(coil -> HardAckNumber(0))),
          coils = List(coil)
        )
        val _ = assert(c.outcome.isRight, s"replay failed: ${c.outcome}")
        val fed = c.coilAckSeq.collect { case h: HardAck => h.hardAckNum: Int }
        val _ = assert(fed == Vector(1, 2), s"gap = acks above the mark (1,2), none <= 0; got $fed")
    }

    test(
      "ReplayActor.replay (coil) routes the coil tail (coil anchors, HubHardAck + own coil HardAck)"
    ) {
        val c = runReplayCoil(seedRecoverableCoil)
        val _ = assert(c.outcome.isRight, s"coil replay failed: ${c.outcome}")
        // BlockWeaver: first PollResults + the block brief (>= coilBlockMark+1 = 3).
        val _ = assert(c.bw.exists(_.isInstanceOf[PollResults]), "BW PollResults")
        val _ = assert(hasBlock(c.bw, 5), "BW block 5")
        // FastConsensusActor: the block brief (aggregator floor softConfirmed+1 = 0).
        val _ = assert(hasBlock(c.fca, 5), "FCA block 5")
        // SlowConsensusActor: the reconstructed in-flight handoff + every coil-quorum hard-ack —
        // this coil peer's own two coil HardAcks (stack 1) routed back, plus the hub's HubHardAck
        // (unwrapped to its `.ack`).
        val _ = assert(c.sca.exists(_.isInstanceOf[SlowConsensusActor.StackHandoff]), "SCA handoff")
        val _ = assert(
          c.sca.count(_.isInstanceOf[HardAck]) == 3,
          "SCA 3 hard-acks (2 own coil + 1 hub)"
        )
        // StackComposer: only stack >= coilHardAckedStack+1 = 2 (stack 1 is the in-flight band).
        assert(
          c.sc.collect { case b: StackBrief => b.stackNum } == Vector(StackNumber(2)),
          "SC only stack 2"
        )
    }

    // ---- harness ----

    private final case class Captured(
        bw: Vector[BlockWeaver.Request],
        fca: Vector[FastConsensusActor.Request],
        sca: Vector[SlowConsensusActor.Request],
        sc: Vector[StackComposer.Request],
        coilAckSeq: Vector[CoilAckSequencer.Request],
        outcome: Either[Throwable, Unit]
    )

    /** Boot four probe targets + a mock L1, seed the store, run `replay`, settle, and capture both
      * each probe's received messages and the replay outcome.
      */
    private def runReplay(
        seed: Persistence[IO] => IO[Unit],
        hubs: List[HeadPeerNumber] = Nil,
        coils: List[CoilPeerNumber] = Nil
    ): Captured =
        val own = ownNum
        val peers = config.headPeerIds.map(_.peerNum).toList
        val treasuryAddress = config.initializationTx.treasuryProduced.address
        val persistenceTracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
        InMemoryBackendStore
            .open(persistenceTracer)
            .use(backend =>
                ActorSystem[IO]("replay-test").use(system =>
                    for {
                        persistence <- Persistence.fromBackend(backend, persistenceTracer)
                        cardanoBackend <- CardanoBackendMock.mockIO(MockState(Map.empty))
                        bwSink <- Ref.of[IO, Vector[BlockWeaver.Request]](Vector.empty)
                        fcaSink <- Ref.of[IO, Vector[FastConsensusActor.Request]](Vector.empty)
                        scaSink <- Ref.of[IO, Vector[SlowConsensusActor.Request]](Vector.empty)
                        scSink <- Ref.of[IO, Vector[StackComposer.Request]](Vector.empty)
                        bw <- system.actorOf(Recorder[BlockWeaver.Request](bwSink))
                        fca <- system.actorOf(Recorder[FastConsensusActor.Request](fcaSink))
                        sca <- system.actorOf(Recorder[SlowConsensusActor.Request](scaSink))
                        sc <- system.actorOf(Recorder[StackComposer.Request](scSink))
                        seqSink <- Ref.of[IO, Vector[CoilAckSequencer.Request]](Vector.empty)
                        seq <- system.actorOf(Recorder[CoilAckSequencer.Request](seqSink))
                        _ <- seed(persistence)
                        outcome <- ReplayActor
                            .replay(
                              persistence,
                              cardanoBackend,
                              ReplayActor.Targets(bw, fca, sca, sc, Some(seq)),
                              PeerId.Head(own),
                              peers,
                              hubs,
                              coils,
                              treasuryAddress
                            )
                            .attempt
                        _ <- IO.sleep(1.second) // let the probe fibers drain their mailboxes
                        bwMsgs <- bwSink.get
                        fcaMsgs <- fcaSink.get
                        scaMsgs <- scaSink.get
                        scMsgs <- scSink.get
                        seqMsgs <- seqSink.get
                    } yield Captured(bwMsgs, fcaMsgs, scaMsgs, scMsgs, seqMsgs, outcome)
                )
            )
            .unsafeRunSync()

    /** Coil counterpart of [[runReplay]]: boot four probes + a mock L1, seed, run `replay` with
      * coil params (own coil 0, all head peers, a single hub head peer 1), settle, capture.
      */
    private def runReplayCoil(seed: Persistence[IO] => IO[Unit]): Captured =
        val peers = config.headPeerIds.map(_.peerNum).toList
        val hubs = List(HeadPeerNumber(1))
        val treasuryAddress = config.initializationTx.treasuryProduced.address
        val persistenceTracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
        InMemoryBackendStore
            .open(persistenceTracer)
            .use(backend =>
                ActorSystem[IO]("replay-coil-test").use(system =>
                    for {
                        persistence <- Persistence.fromBackend(backend, persistenceTracer)
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
                              PeerId.Coil(CoilPeerNumber(0)),
                              peers,
                              hubs,
                              Nil,
                              treasuryAddress
                            )
                            .attempt
                        _ <- IO.sleep(1.second) // let the probe fibers drain their mailboxes
                        bwMsgs <- bwSink.get
                        fcaMsgs <- fcaSink.get
                        scaMsgs <- scaSink.get
                        scMsgs <- scSink.get
                    } yield Captured(bwMsgs, fcaMsgs, scaMsgs, scMsgs, Vector.empty, outcome)
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
            _ <- p.put(JournalKey.HardAck(PeerId.Head(own), HardAckNumber(0)))(
              JournalValue(stamp, hardAck(own.convert, 0, 1))
            )
            _ <- p.put(JournalKey.HardAck(PeerId.Head(own), HardAckNumber(1)))(
              JournalValue(stamp, hardAck(own.convert, 1, 1))
            )
            // A remote peer's hard-ack for stack 1 → routed to SCA via the tail.
            _ <- p.put(JournalKey.HardAck(PeerId.Head(other), HardAckNumber(0)))(
              JournalValue(stamp, hardAck(other.convert, 0, 1))
            )
            // The in-flight stack's unsigned form → reconstructed handoff to SCA.
            unsigned <- unsignedStack(1)
            _ <- p.put(StoreKey.UnsignedStack(StackNumber(1)))(unsigned)
            // A block brief → FCA + BlockWeaver.
            block5 <- blockBrief(5)
            _ <- p.put(JournalKey.Block(BlockNumber(5)))(JournalValue(stamp, block5))
            // Two stack briefs → SC gets only stack 2 (>= composer floor 2).
            s1 <- stackBrief(1, 0, 0)
            s2 <- stackBrief(2, 1, 1)
            _ <- p.put(JournalKey.Stack(StackNumber(1)))(JournalValue(stamp, s1))
            _ <- p.put(JournalKey.Stack(StackNumber(2)))(JournalValue(stamp, s2))
        } yield ()

    /** softConfirmed = 5 but fastBlockMark = max(BlockResult) = 2 → confirmed > acked, a torn
      * store. `validateInvariants` reads only the SoftConfirmation key and the BlockResult key, so
      * a dummy value byte suffices for the former and a real `BlockResult` for the latter.
      */
    private def seedInconsistent(p: Persistence[IO]): IO[Unit] =
        for {
            _ <- p.backend.put(
              Cf.SoftConfirmation,
              StoreKey.SoftConfirmation(BlockNumber(5)).encode,
              Array[Byte](0)
            )
            br <- blockResult(2)
            _ <- p.put(StoreKey.BlockResult(BlockNumber(2)))(br.persisted)
        } yield ()

    /** Coil store: stack 0 hard-confirmed (`hardConfirmed = 0`); stack 1 acked-not-confirmed via
      * the coil peer's own two coil `HardAck`s + its `UnsignedStack`; the hub's `HubHardAck`
      * carrying a second coil peer's stack-1 ack; a `BlockResult` (the coil fast anchor
      * `coilBlockMark = 2`) + its `RequestHighWater`; a block brief; and two stack briefs (1 + 2)
      * for the composer floor.
      */
    private def seedRecoverableCoil(p: Persistence[IO]): IO[Unit] =
        val coil = CoilPeerNumber(0)
        val hub = HeadPeerNumber(1)
        for {
            _ <- p.backend.put(
              Cf.HardConfirmation,
              StoreKey.HardConfirmation(StackNumber(0)).encode,
              Array[Byte](0)
            )
            // Own coil hard-acks for stack 1 → coilHardAcked = 1, coilHardAckedStack = 1 (> 0).
            _ <- p.put(JournalKey.HardAck(PeerId.Coil(coil), HardAckNumber(0)))(
              JournalValue(stamp, coilHardAck(0, 0, 1))
            )
            _ <- p.put(JournalKey.HardAck(PeerId.Coil(coil), HardAckNumber(1)))(
              JournalValue(stamp, coilHardAck(0, 1, 1))
            )
            // The hub's re-sequenced coil-quorum ack (another coil peer's stack-1 ack) → SCA via
            // its `.ack`.
            _ <- p.put(JournalKey.HubHardAck(hub, HubHardAckNumber(0)))(
              JournalValue(stamp, hubHardAck(1, 0, coilHardAck(1, 0, 1)))
            )
            unsigned <- unsignedStack(1)
            _ <- p.put(StoreKey.UnsignedStack(StackNumber(1)))(unsigned)
            // The coil fast anchor: max(BlockResult) = 2 (replay reads only the key), with its
            // request high-water.
            br <- blockResult(2)
            _ <- p.put(StoreKey.BlockResult(BlockNumber(2)))(br.persisted)
            _ <- p.put(StoreKey.RequestHighWater(BlockNumber(2)))(
              Map.empty[HeadPeerNumber, RequestNumber]
            )
            block5 <- blockBrief(5)
            _ <- p.put(JournalKey.Block(BlockNumber(5)))(JournalValue(stamp, block5))
            s1 <- stackBrief(1, 0, 0)
            s2 <- stackBrief(2, 1, 1)
            _ <- p.put(JournalKey.Stack(StackNumber(1)))(JournalValue(stamp, s1))
            _ <- p.put(JournalKey.Stack(StackNumber(2)))(JournalValue(stamp, s2))
        } yield ()

    /** A single hub's `HubHardAck` (a coil peer's ack re-sequenced by hub 1) — the only durable
      * datum, so a head peer's replay (with hub 1 in `hubs`) scans it and feeds its `.ack` to SCA,
      * exercising head-peer coil-quorum recovery (P14).
      */
    private def seedHubHardAck(p: Persistence[IO]): IO[Unit] =
        p.put(JournalKey.HubHardAck(HeadPeerNumber(1), HubHardAckNumber(0)))(
          JournalValue(stamp, hubHardAck(1, 0, coilHardAck(0, 0, 1)))
        )

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
              BlockBody.Minor(requests = List.empty, depositsRejected = List.empty)
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

    private def coilHardAck(coil: Int, ackNum: Int, stack: Int): HardAck =
        HardAck(
          ackId = HardAckId(PeerId.Coil(CoilPeerNumber(coil)), HardAckNumber(ackNum)),
          stackNum = StackNumber(stack),
          payload = HardAck.Round2Payload.Regular(TxSignature(IArray.from(Array.fill[Byte](64)(0))))
        )

    private def hubHardAck(hub: Int, seq: Int, ack: HardAck): HardAckWithId =
        HardAckWithId(hubPeer = HeadPeerNumber(hub), seqNum = HubHardAckNumber(seq), ack = ack)

    private def blockResult(blockNum: Int): IO[BlockResult] =
        for {
            brief <- blockBrief(blockNum)
            now <- realTimeQuantizedInstant(headConfig.slotConfig)
        } yield BlockResult(
          brief = brief,
          evacuationMapDiff = Nil,
          payoutObligations = Nil,
          payoutRequestIds = Nil,
          postDatedRefundTxs = Nil,
          absorbedDeposits = Nil,
          competingFallbackTxTime =
              headConfig.txTiming.newFallbackStartTime(BlockCreationEndTime(now + 1.second))
        )
