package hydrozoa.multisig.consensus

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
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger
import hydrozoa.multisig.ledger.eutxol2.store.InMemoryL2Store
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.{StackBrief, StackEffects, StackNumber}
import hydrozoa.multisig.persistence.{InMemoryBackendStore, JournalKey, JournalValue, Persistence, PersistenceEventFormat, StoreKey, Timestamped}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

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
