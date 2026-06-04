package hydrozoa.multisig.persistence.recovery

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOLocal}
import cats.syntax.all.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.multisig.timing.TxTiming.StackTimes.StackCreationEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.Tracer
import hydrozoa.multisig.consensus.StackComposer
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockResult, BlockVersion}
import hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger
import hydrozoa.multisig.ledger.eutxol2.store.InMemoryL2Store
import hydrozoa.multisig.ledger.joint.{EvacuationMap, JointLedger}
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.l2.{L2CommandNumber, L2LedgerCommand}
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}
import hydrozoa.multisig.persistence.codec.TreasuryFixture
import hydrozoa.multisig.persistence.{ArrivalStamp, InMemoryBackendStore, LaneKey, LaneValue, Markers, Persistence, StoreKey}
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt

/** Tests for the R2-fast recover seams — the pure-over-store reconstruction of `JointLedger`'s and
  * `StackComposer`'s passive state after a crash. Each test seeds a store with the typed values the
  * producers write (`persistOwnAckBundle` / `persistOwnStackClose` / `PeerLiaison.persistInbound`)
  * and asserts the recovered state equals the crash-time value. Markers / `softAcked` are passed
  * directly — the marker-derivation → recover wiring is R3.
  */
class RecoverSeamsTest extends AnyFunSuite:

    /** A deterministic node config (fixed seed) — supplies the L2 ledger's genesis utxos, the head
      * config for building briefs, and the `CardanoNetwork.Section` the store codecs round-trip
      * under.
      */
    private val config: NodeConfig =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(0L))

    private val headConfig: HeadConfig = config.headConfig

    private given CardanoNetwork.Section = config

    private val stamp: ArrivalStamp = ArrivalStamp(generation = 0, monotonicNanos = 1L)

    // ---- JointLedger ----

    test("JointLedger.recover / recoverState return None for an empty store (no own soft-ack)") {
        withStore { p =>
            for
                store <- InMemoryL2Store.create
                ledger <- EutxoL2Ledger(config, store)
                viaRecover <- JointLedger.State.recover(p, ledger, None)
                viaState <- JointLedger.State.recoverState(p, None)
            yield assert(viaRecover.isEmpty && viaState.isEmpty)
        }
    }

    test("JointLedger.recoverState rebuilds Done from the softAcked block brief + deposits") {
        withStore { p =>
            for
                brief <- blockBrief(4)
                _ <- p.put(LaneKey.Block(BlockNumber(4)))(LaneValue(stamp, brief))
                _ <- p.put(StoreKey.DepositMap)(DepositsMap.empty)
                done <- JointLedger.State.recoverState(p, Some(SoftAckNumber(4)))
            yield assert(
              done.exists(d =>
                  d.previousBlockHeader == brief.header && d.deposits == DepositsMap.empty
              )
            )
        }
    }

    test("JointLedger.recover co-anchors the L2 ledger to the softAcked command number") {
        withStore { p =>
            for
                store <- InMemoryL2Store.create
                ledger <- EutxoL2Ledger(config, store)
                // Advance the live ledger past the crash boundary (to command number 3).
                _ <- (1 to 3).toList.traverse_(i =>
                    ledger.sendApplyDepositDecisions(noop(i)).value.flatMap(IO.fromEither)
                )
                // Crash boundary: softAcked = block 2, recorded at L2 command number 2.
                brief <- blockBrief(2)
                _ <- p.put(LaneKey.Block(BlockNumber(2)))(LaneValue(stamp, brief))
                _ <- p.put(StoreKey.DepositMap)(DepositsMap.empty)
                _ <- p.put(StoreKey.L2CommandNumber(BlockNumber(2)))(L2CommandNumber(2L))
                done <- JointLedger.State.recover(p, ledger, Some(SoftAckNumber(2)))
                anchored <- ledger.currentCommandNumber
            yield assert(done.isDefined && anchored == L2CommandNumber(2L))
        }
    }

    // ---- StackComposer ----

    test("StackComposer.recover returns None for an empty store (no own hard-ack)") {
        withStore { p =>
            StackComposer.State
                .recover(p, None, None, HeadPeerNumber(0))
                .map(s => assert(s.isEmpty))
        }
    }

    test(
      "StackComposer.recover rebuilds counters + snapshots from the last own hard-ack " +
          "(HardAckNumber != stackNum)"
    ) {
        withStore { p =>
            val own = HeadPeerNumber(0)
            // The last own hard-ack number is 5, but it belongs to stack 2 — the stack number must
            // come from the hard-ack VALUE, not the marker number.
            val hardAckNum = 5
            val stackN = 2
            val lastBlock = 7
            for
                sb <- stackBrief(stack = stackN, firstBlock = 4, lastBlock = lastBlock)
                _ <- p.put(LaneKey.HardAck(own, HardAckNumber(hardAckNum)))(
                  LaneValue(stamp, hardAck(peer = 0, ackNum = hardAckNum, stack = stackN))
                )
                _ <- p.put(LaneKey.Stack(StackNumber(stackN)))(LaneValue(stamp, sb))
                _ <- p.put(StoreKey.Treasury)(TreasuryFixture.sampleTreasury)
                _ <- p.put(StoreKey.EvacuationMap(BlockNumber(lastBlock)))(EvacuationMap.empty)
                markers = Markers(
                  softConfirmed = None,
                  hardConfirmed = Some(StackNumber(stackN)), // stack 2 hard-confirmed → gate armed
                  softAcked = None,
                  hardAcked = Some(HardAckNumber(hardAckNum))
                )
                recovered <- StackComposer.State.recover(
                  p,
                  markers.hardAcked,
                  markers.hardConfirmed,
                  own
                )
            yield assert(
              recovered.exists { s =>
                  s.lastClosedStackNum == StackNumber(stackN) &&
                  s.lastClosedBlockNum == BlockNumber(lastBlock) &&
                  s.nextOwnHardAckNum == HardAckNumber(hardAckNum + 1) &&
                  s.previousStackHardConfirmed &&
                  s.treasury == TreasuryFixture.sampleTreasury &&
                  s.evacuationMap == EvacuationMap.empty &&
                  s.pending.isEmpty &&
                  s.ready.isEmpty
              }
            )
        }
    }

    test(
      "StackComposer.recover rebuilds pending from BlockResults strictly after the last closed " +
          "block; gate disarmed when the last stack is not yet hard-confirmed"
    ) {
        withStore { p =>
            val own = HeadPeerNumber(0)
            val stackN = 1
            val lastBlock = 3
            for
                sb <- stackBrief(stack = stackN, firstBlock = 0, lastBlock = lastBlock)
                _ <- p.put(LaneKey.HardAck(own, HardAckNumber(0)))(
                  LaneValue(stamp, hardAck(peer = 0, ackNum = 0, stack = stackN))
                )
                _ <- p.put(LaneKey.Stack(StackNumber(stackN)))(LaneValue(stamp, sb))
                _ <- p.put(StoreKey.Treasury)(TreasuryFixture.sampleTreasury)
                _ <- p.put(StoreKey.EvacuationMap(BlockNumber(lastBlock)))(EvacuationMap.empty)
                // A stale BlockResult at the last closed block must be excluded (scan is exclusive).
                br3 <- blockResult(3)
                br4 <- blockResult(4)
                br5 <- blockResult(5)
                _ <- p.put(StoreKey.BlockResult(BlockNumber(3)))(br3)
                _ <- p.put(StoreKey.BlockResult(BlockNumber(4)))(br4)
                _ <- p.put(StoreKey.BlockResult(BlockNumber(5)))(br5)
                markers = Markers(
                  softConfirmed = None,
                  hardConfirmed = Some(StackNumber(0)), // below stack 1 → gate disarmed
                  softAcked = None,
                  hardAcked = Some(HardAckNumber(0))
                )
                recovered <- StackComposer.State.recover(
                  p,
                  markers.hardAcked,
                  markers.hardConfirmed,
                  own
                )
            yield assert(
              recovered.exists { s =>
                  s.pending.keySet == Set(BlockNumber(4), BlockNumber(5)) &&
                  !s.previousStackHardConfirmed
              }
            )
        }
    }

    // ---- corruption (fail-safe) ----

    test("JointLedger.recoverState throws on a non-empty store missing the softAcked block brief") {
        withStore { p =>
            JointLedger.State
                .recoverState(p, Some(SoftAckNumber(2)))
                .attempt
                .map(r => assert(r.swap.toOption.exists(_.isInstanceOf[IllegalStateException])))
        }
    }

    test("JointLedger.recover throws when the softAcked block's L2 command number is missing") {
        withStore { p =>
            for
                store <- InMemoryL2Store.create
                ledger <- EutxoL2Ledger(config, store)
                brief <- blockBrief(2)
                _ <- p.put(LaneKey.Block(BlockNumber(2)))(LaneValue(stamp, brief))
                _ <- p.put(StoreKey.DepositMap)(DepositsMap.empty)
                // L2CommandNumber intentionally not written
                r <- JointLedger.State.recover(p, ledger, Some(SoftAckNumber(2))).attempt
            yield assert(r.swap.toOption.exists(_.isInstanceOf[IllegalStateException]))
        }
    }

    test("StackComposer.recover throws on a non-empty store missing the Treasury snapshot") {
        withStore { p =>
            val own = HeadPeerNumber(0)
            for
                sb <- stackBrief(stack = 1, firstBlock = 0, lastBlock = 3)
                _ <- p.put(LaneKey.HardAck(own, HardAckNumber(0)))(
                  LaneValue(stamp, hardAck(peer = 0, ackNum = 0, stack = 1))
                )
                _ <- p.put(LaneKey.Stack(StackNumber(1)))(LaneValue(stamp, sb))
                // Treasury intentionally not written
                markers = Markers(None, Some(StackNumber(1)), None, Some(HardAckNumber(0)))
                r <- StackComposer.State
                    .recover(p, markers.hardAcked, markers.hardConfirmed, own)
                    .attempt
            yield assert(r.swap.toOption.exists(_.isInstanceOf[IllegalStateException]))
        }
    }

    // ---- fixtures ----

    private def noop(n: Int): L2LedgerCommand.ApplyDepositDecisions =
        L2LedgerCommand.ApplyDepositDecisions(
          blockNumber = BlockNumber(n),
          blockCreationEndTime = BigInt(n),
          absorbedDeposits = Nil,
          refundedDeposits = Nil
        )

    /** A minimal `BlockBrief.Next` (a Minor) at `blockNum` — mirrors `BlockWeaverTest`'s builder.
      */
    private def blockBrief(blockNum: Int): IO[BlockBrief.Next] =
        realTimeQuantizedInstant(headConfig.slotConfig).map(minorBrief(blockNum, _))

    private def minorBrief(blockNum: Int, now: QuantizedInstant): BlockBrief.Next = {
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
          BlockBody.Minor(events = List.empty, depositsRefunded = List.empty)
        )
    }

    private def blockResult(blockNum: Int): IO[BlockResult] =
        realTimeQuantizedInstant(headConfig.slotConfig).map { now =>
            val end = BlockCreationEndTime(now + 1.second)
            BlockResult(
              brief = minorBrief(blockNum, now),
              evacuationMapDiff = Nil,
              payoutObligations = Nil,
              postDatedRefundTxs = Nil,
              absorbedDeposits = Nil,
              competingFallbackTxTime = headConfig.txTiming.newFallbackStartTime(end)
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

    /** A hard-ack with a chosen `stackNum` — payload is the simplest round-2 shape (recover reads
      * only `stackNum`).
      */
    private def hardAck(peer: Int, ackNum: Int, stack: Int): HardAck =
        HardAck(
          ackId = HardAckId(HeadPeerNumber(peer), HardAckNumber(ackNum)),
          stackNum = StackNumber(stack),
          payload = HardAck.Round2Payload.Regular(
            TxSignature(IArray.from(Array.fill[Byte](64)(0)))
          )
        )

    private def withStore(prog: Persistence[IO] => IO[Assertion]): Assertion =
        (for
            tracerLocal <- Tracer.makeLocal
            result <- {
                given IOLocal[Tracer] = tracerLocal
                InMemoryBackendStore.open.use(backend =>
                    Persistence.fromBackend(backend).flatMap(prog)
                )
            }
        yield result).unsafeRunSync()
