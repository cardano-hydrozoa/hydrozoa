package hydrozoa.multisig.consensus

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.contravariant.*
import hydrozoa.config.head.multisig.timing.TxTiming.StackTimes.StackCreationEndTime
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber}
import hydrozoa.multisig.consensus.liaison.BatchMessages.Join
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.{BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger
import hydrozoa.multisig.ledger.eutxol2.store.InMemoryL2Store
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap
import hydrozoa.multisig.ledger.l1.tx.{SettlementTx, TxSignature, genSettlementTxSeqBuilder}
import hydrozoa.multisig.ledger.l1.txseq.SettlementTxSeq
import hydrozoa.multisig.ledger.l2.{L2CommandNumber, L2StateHash}
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StackBrief, StackEffects, StackNumber, StandaloneEvacuationCommitment}
import hydrozoa.multisig.persistence.{InMemoryBackendStore, JournalKey, JournalValue, Persistence, PersistenceEventFormat, StoreKey, Timestamped}
import hydrozoa.rulebased.ledger.l1.state.StandaloneEvacuationCommitmentOnchain
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString
import test.MinorBlocks

/** What a hub decides when a coil connects — [[CoilStartPoint.decide]].
  *
  * ⚠️ The decision that matters most here is the **common** one. A hub produces continuously, so a
  * reconnecting coil is essentially always behind; if the threshold were wrong in the tightening
  * direction every brief disconnect would trigger a full state transfer, and nothing would look
  * broken — it would just be needlessly expensive. The boundary cases below are what pin it.
  */
class CoilStartPointTest extends AnyFunSuite:

    private val multiNodeConfig: MultiNodeConfig =
        MultiNodeConfig.generateDefault.pureApply(Gen.Parameters.default, Seed(0L))

    private val nodeConfig: NodeConfig = multiNodeConfig.nodeConfigs(HeadPeerNumber.zero)

    private given CoilStartPoint.Config = nodeConfig

    private val threshold: Int = nodeConfig.coilCatchUpStacks

    private val coil: PeerId.Coil = PeerId.Coil(CoilPeerNumber(0))

    /** Stack 0's hard-confirmed effects — `Initial`, so it carries no settlement. The cheapest real
      * effects value to seed a store with.
      */
    private val initialEffects: StackEffects.HardConfirmed =
        StackEffects.HardConfirmed.Initial(
          initializationTx = nodeConfig.headConfig.initialBlock.effects.initializationTx,
          fallbackTx = nodeConfig.headConfig.initialBlock.effects.fallbackTx
        )

    private def stackBrief(stack: Int, lastBlock: Int): IO[StackBrief] =
        realTimeQuantizedInstant(nodeConfig.headConfig.slotConfig).map(now =>
            StackBrief(
              stackNum = StackNumber(stack),
              firstBlockNum = BlockNumber(lastBlock),
              lastBlockNum = BlockNumber(lastBlock),
              creationEndTime = StackCreationEndTime(now)
            )
        )

    /** An own-ack from the COIL, not a head peer — this is the lane the hub pulls from it. */
    private def coilAck(ackNum: Int, stack: Int): HardAck =
        HardAck(
          ackId = HardAckId(coil, HardAckNumber(ackNum)),
          stackNum = StackNumber(stack),
          payload = HardAck.Round2Payload.Regular(TxSignature(IArray.from(Array.fill[Byte](64)(0))))
        )

    private def hardAck(ackNum: Int, stack: Int): HardAck =
        HardAck(
          ackId = HardAckId(PeerId.Head(HeadPeerNumber(0)), HardAckNumber(ackNum)),
          stackNum = StackNumber(stack),
          payload = HardAck.Round2Payload.Regular(TxSignature(IArray.from(Array.fill[Byte](64)(0))))
        )

    /** Seed a store with hard-confirmations for stacks `1..upTo`, each `Initial`-shaped (no
      * settlement anywhere), plus their briefs. Enough for every decision that does not reach the
      * certificate.
      */
    private def seedStacks(p: Persistence[IO], upTo: Int): IO[Unit] =
        (1 to upTo).toList.foldLeft(IO.unit) { (acc, n) =>
            acc.flatMap(_ =>
                for {
                    stamp <- p.arrivalStamp
                    brief <- stackBrief(n, n)
                    _ <- p.put(StoreKey.HardConfirmation(StackNumber(n)))(
                      Timestamped(stamp, initialEffects)
                    )
                    _ <- p.put(JournalKey.Stack(StackNumber(n)))(JournalValue(stamp, brief))
                } yield ()
            )
        }

    private def decide(p: Persistence[IO], coilStack: Option[Int]): IO[CoilStartPoint] =
        for {
            store <- InMemoryL2Store.create
            ledger <- EutxoL2Ledger(nodeConfig, store)
            result <- CoilStartPoint.decide(
              coil,
              Join.Connected(block = None, stack = coilStack.map(StackNumber(_))),
              p,
              ledger
            )
        } yield result

    private def withStore(prog: Persistence[IO] => IO[Assertion]): Assertion =
        val tracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
        InMemoryBackendStore
            .open(tracer)
            .use(backend => Persistence.fromBackend(backend, tracer).flatMap(prog))
            .unsafeRunSync()

    test("an empty store has nothing to seed from") {
        withStore(p =>
            decide(p, coilStack = None).map(r =>
                assert(r == CoilStartPoint.Unavailable(CoilStartPoint.Reason.HeadAtStackZero))
            )
        )
    }

    test("a head still at stack 0 has nothing to seed from") {
        withStore(p =>
            for {
                stamp <- p.arrivalStamp
                _ <- p.put(StoreKey.HardConfirmation(StackNumber.zero))(
                  Timestamped(stamp, initialEffects)
                )
                r <- decide(p, coilStack = None)
            } yield assert(r == CoilStartPoint.Unavailable(CoilStartPoint.Reason.HeadAtStackZero))
        )
    }

    test("a coil exactly at the hub's latest stack catches up") {
        withStore(p =>
            for {
                _ <- seedStacks(p, upTo = 20)
                r <- decide(p, coilStack = Some(20))
            } yield assert(r == CoilStartPoint.CatchUp)
        )
    }

    test("a coil exactly `coilCatchUpStacks` behind still catches up — the boundary is inclusive") {
        withStore(p =>
            for {
                _ <- seedStacks(p, upTo = 40)
                r <- decide(p, coilStack = Some(40 - threshold))
            } yield assert(r == CoilStartPoint.CatchUp)
        )
    }

    test("a coil one stack past the threshold is seeded, not left to catch up") {
        withStore(p =>
            for {
                _ <- seedStacks(p, upTo = 40)
                r <- decide(p, coilStack = Some(40 - threshold - 1))
                // No settlement anywhere in this store, so it stops at the certificate — but it got
                // PAST the catch-up decision, which is what this pins.
            } yield assert(
              r == CoilStartPoint.Unavailable(CoilStartPoint.Reason.NoMajorYet(StackNumber(40)))
            )
        )
    }

    test("a coil reporting no stack at all is never within catch-up") {
        withStore(p =>
            for {
                // One stack, so the hub is barely ahead — well inside any threshold. A coil with a
                // mark here would catch up; one holding nothing must not.
                _ <- seedStacks(p, upTo = 1)
                r <- decide(p, coilStack = None)
            } yield assert(
              r == CoilStartPoint.Unavailable(CoilStartPoint.Reason.NoMajorYet(StackNumber(1)))
            )
        )
    }

    test("a head with stacks but no major cannot seed") {
        withStore(p =>
            for {
                _ <- seedStacks(p, upTo = 40)
                r <- decide(p, coilStack = None)
            } yield assert(
              r == CoilStartPoint.Unavailable(CoilStartPoint.Reason.NoMajorYet(StackNumber(40)))
            )
        )
    }

    // ---- the certificate path: a store that actually has a major -------------------------------

    /** A settlement seq built against this head's config, multisigned by every head peer. */
    private val settlementSeq: SettlementTxSeq =
        genSettlementTxSeqBuilder(multiNodeConfig.headConfig)()
            .pureApply(Gen.Parameters.default, Seed(1L))
            .result match {
            case Left(e)  => throw RuntimeException(s"settlement build failed: $e")
            case Right(s) => s
        }

    private val signedSettlement: SettlementTx =
        val unsigned = settlementSeq.settlementTx
        unsigned.txLens.replace(multiNodeConfig.multisignTx(unsigned.tx))(unsigned)

    /** Effects for a **major** stack — a settlement, and no SEC beside it. */
    private val majorEffects: StackEffects.HardConfirmed =
        StackEffects.HardConfirmed.Regular(
          NonEmptyList.of(
            PartitionEffects.Major(
              settlement = signedSettlement,
              fallback = settlementSeq.fallbackTx,
              rollouts = Nil,
              refunds = Nil,
              sec = None
            )
          )
        )

    private val minorSec: StandaloneEvacuationCommitment.MultiSigned =
        StandaloneEvacuationCommitment.MultiSigned(
          commitment = StandaloneEvacuationCommitment(
            blockNum = BlockNumber(1),
            blockVersion = BlockVersion.Full(1, 1),
            kzgCommitment = EvacuationMap.empty.kzgCommitment,
            l2StateHash = L2StateHash(ByteString.fromArray(Array.fill[Byte](32)(0x5c.toByte))),
            header = StandaloneEvacuationCommitmentOnchain(
              StandaloneEvacuationCommitmentOnchain(
                headId = multiNodeConfig.headConfig.headTokenNames.treasuryTokenName.bytes,
                versionMajor = 1,
                versionMinor = 1,
                commitment = EvacuationMap.empty.kzgCommitment,
                l2StateHash = ByteString.fromArray(Array.fill[Byte](32)(0x5c.toByte))
              )
            )
          ),
          signatures = Nil
        )

    /** Effects for a **minor-only** stack — a mandatory SEC, no settlement of its own. */
    private val minorEffects: StackEffects.HardConfirmed =
        StackEffects.HardConfirmed.Regular(
          NonEmptyList.of(PartitionEffects.Minor(sec = minorSec, refunds = Nil))
        )

    /** Seed one stack with given effects, plus everything `buildOffer` reads at its last block. */
    private def seedStack(
        p: Persistence[IO],
        stack: Int,
        effects: StackEffects.HardConfirmed
    ): IO[Unit] =
        for {
            stamp <- p.arrivalStamp
            brief <- stackBrief(stack, stack)
            _ <- p.put(StoreKey.HardConfirmation(StackNumber(stack)))(Timestamped(stamp, effects))
            _ <- p.put(JournalKey.Stack(StackNumber(stack)))(JournalValue(stamp, brief))
            _ <- p.put(StoreKey.L2CommandNumber(BlockNumber(stack)))(L2CommandNumber(0L))
            _ <- p.put(StoreKey.RequestHighWater(BlockNumber(stack)))(Map.empty)
            // The fast-side anchor the offer carries: the brief of the stack's last block and the
            // deposit map at it. A hub that never wrote these cannot seed anyone.
            block <- MinorBlocks.brief(nodeConfig.headConfig, stack)
            _ <- p.put(JournalKey.Block(BlockNumber(stack)))(JournalValue(stamp, block))
            _ <- p.put(StoreKey.DepositMap(BlockNumber(stack)))(DepositsMap.empty)
        } yield ()

    test("a major start point offers its own settlement and no SEC") {
        withStore(p =>
            for {
                _ <- seedStack(p, stack = 1, majorEffects)
                r <- decide(p, coilStack = None)
            } yield r match {
                case CoilStartPoint.Offer(offer) =>
                    assert(offer.startStack == StackNumber(1))
                    assert(offer.sec.isEmpty, "a major carries its own settlement; no SEC needed")
                    assert(offer.settlement.tx.id == signedSettlement.tx.id)
                case other => fail(s"expected an Offer, got $other")
            }
        )
    }

    test("an offer carries the fast-side anchor one block below where the cursors open") {
        // Without these the coil can read the population lane and still not build a block: block
        // `n+1` is built on `n`'s header, and `n` is the one block the cursors skip.
        withStore(p =>
            for {
                _ <- seedStack(p, stack = 1, majorEffects)
                r <- decide(p, coilStack = None)
            } yield r match {
                case CoilStartPoint.Offer(offer) =>
                    val _ = assert(
                      offer.block.blockNum == BlockNumber(1),
                      "the anchor block must be the stack's last, not the cursor's first"
                    )
                    assert(
                      (offer.cursors.block: Int) == (offer.block.blockNum: Int) + 1,
                      s"cursors open at ${offer.cursors.block}, anchor is ${offer.block.blockNum}"
                    )
                case other => fail(s"expected an Offer, got $other")
            }
        )
    }

    test("a minor start point walks back for the latest major's settlement and ships the SEC") {
        withStore(p =>
            for {
                _ <- seedStack(p, stack = 1, majorEffects)
                _ <- seedStack(p, stack = 2, minorEffects)
                r <- decide(p, coilStack = None)
            } yield r match {
                case CoilStartPoint.Offer(offer) =>
                    assert(offer.startStack == StackNumber(2), "the start point is the minor")
                    // ⚠️ Compared field-wise, NOT with `contains`/`==`. A SEC.s `header` is an
                    // opaque `IArray[Byte]`, so case-class equality on `MultiSigned` is reference
                    // equality on that field — two values that print identically compare unequal
                    // after a store round-trip.
                    assert(
                      offer.sec.map(_.commitment.kzgCommitment) ==
                          Some(minorSec.commitment.kzgCommitment),
                      "the minor's own SEC must travel"
                    )
                    assert(
                      offer.sec.map(_.commitment.blockNum) == Some(minorSec.commitment.blockNum)
                    )
                    assert(
                      offer.settlement.tx.id == signedSettlement.tx.id,
                      "the treasury comes from the earlier major, which a minor does not rotate"
                    )
                case other => fail(s"expected an Offer, got $other")
            }
        )
    }

    test("the coil's first own-ack index is the first one past the start point") {
        withStore(p =>
            for {
                _ <- seedStack(p, stack = 1, majorEffects)
                stamp <- p.arrivalStamp
                // Acks 0 and 1 cover stack 1; ack 2 is the first past it. A coil seeded at stack 1
                // must be asked for 2 — never 0 or 1, which it cannot produce and must not re-sign.
                _ <- p.put(JournalKey.HardAck(coil, HardAckNumber(0)))(
                  JournalValue(stamp, coilAck(0, stack = 1))
                )
                _ <- p.put(JournalKey.HardAck(coil, HardAckNumber(1)))(
                  JournalValue(stamp, coilAck(1, stack = 1))
                )
                _ <- p.put(JournalKey.HardAck(coil, HardAckNumber(2)))(
                  JournalValue(stamp, coilAck(2, stack = 2))
                )
                r <- decide(p, coilStack = None)
            } yield r match {
                case CoilStartPoint.Offer(offer) =>
                    assert(offer.ownHardAck == HardAckNumber(2))
                case other => fail(s"expected an Offer, got $other")
            }
        )
    }

    test("with no ack past the start point, the coil is asked for the next index the hub has") {
        withStore(p =>
            for {
                _ <- seedStack(p, stack = 1, majorEffects)
                stamp <- p.arrivalStamp
                _ <- p.put(JournalKey.HardAck(coil, HardAckNumber(0)))(
                  JournalValue(stamp, coilAck(0, stack = 1))
                )
                r <- decide(p, coilStack = None)
            } yield r match {
                case CoilStartPoint.Offer(offer) =>
                    assert(offer.ownHardAck == HardAckNumber(1), "one past the hub's last")
                case other => fail(s"expected an Offer, got $other")
            }
        )
    }
