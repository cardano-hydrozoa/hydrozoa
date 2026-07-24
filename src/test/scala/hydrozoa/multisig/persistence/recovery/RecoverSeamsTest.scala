package hydrozoa.multisig.persistence.recovery

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.multisig.timing.TxTiming.StackTimes.StackCreationEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.{CardanoLiaison, StackComposer}
import hydrozoa.multisig.ledger.block.{BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockResult, BlockVersion}
import hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger
import hydrozoa.multisig.ledger.eutxol2.store.InMemoryL2Store
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.joint.{EvacuationMap, JointLedger}
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.l2.{L2CommandNumber, L2LedgerCommand}
import hydrozoa.multisig.ledger.stack.{PartitionEffects, Stack, StackBrief, StackEffects, StackNumber, StandaloneEvacuationCommitment}
import hydrozoa.multisig.persistence.codec.TreasuryFixture
import hydrozoa.multisig.persistence.{ArrivalStamp, Cf, InMemoryBackendStore, JournalKey, JournalValue, Markers, Persistence, PersistenceEventFormat, StoreKey}
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt
import scalus.uplc.builtin.ByteString

/** Tests for the R2-fast recover seams — the pure-over-store reconstruction of `JointLedger`'s and
  * `StackComposer`'s passive state after a crash. Each test seeds a store with the typed values the
  * producers write (`persistOwnAckBundle` / `persistOwnStackClose` /
  * `PeerLiaisonHeadToHead.persistInbound`) and asserts the recovered state equals the crash-time
  * value. Markers / `fastBlockMark` are passed directly — the marker-derivation → recover wiring is
  * R3.
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

    /** This node's head peer number (the fixture config is always a head node). */
    private val ownNum: HeadPeerNumber = config.ownPeerId match {
        case PeerId.Head(n) => n
        case PeerId.Coil(_) => fail("fixture config must be a head node")
    }

    // ---- JointLedger ----

    test("JointLedger.recover / recoverState return None for an empty store (no BlockResult)") {
        withStore { p =>
            for
                store <- InMemoryL2Store.create
                ledger <- EutxoL2Ledger(config, store)
                viaRecover <- JointLedger.State.recover(p, ledger, None)
                viaState <- JointLedger.State.recoverState(p, None)
            yield assert(viaRecover.isEmpty && viaState.isEmpty)
        }
    }

    test("JointLedger.recoverState rebuilds Done from the fastBlockMark block brief + deposits") {
        withStore { p =>
            for
                brief <- blockBrief(4)
                _ <- p.put(JournalKey.Block(BlockNumber(4)))(JournalValue(stamp, brief))
                _ <- p.put(StoreKey.DepositMap)(DepositsMap.empty)
                done <- JointLedger.State.recoverState(p, Some(BlockNumber(4)))
            yield assert(
              done.exists(d =>
                  d.previousBlockHeader == brief.header && d.deposits == DepositsMap.empty
              )
            )
        }
    }

    test("JointLedger.recover co-anchors the L2 ledger to the fastBlockMark command number") {
        withStore { p =>
            for
                store <- InMemoryL2Store.create
                ledger <- EutxoL2Ledger(config, store)
                // Advance the live ledger past the crash boundary (to command number 3).
                _ <- (1 to 3).toList.traverse_(i =>
                    ledger.sendApplyDepositDecisions(noop(i)).value.flatMap(IO.fromEither)
                )
                // Crash boundary: fastBlockMark = block 2, recorded at L2 command number 2.
                brief <- blockBrief(2)
                _ <- p.put(JournalKey.Block(BlockNumber(2)))(JournalValue(stamp, brief))
                _ <- p.put(StoreKey.DepositMap)(DepositsMap.empty)
                _ <- p.put(StoreKey.L2CommandNumber(BlockNumber(2)))(L2CommandNumber(2L))
                done <- JointLedger.State.recover(p, ledger, Some(BlockNumber(2)))
                anchored <- ledger.currentCommandNumber
            yield assert(done.isDefined && anchored == L2CommandNumber(2L))
        }
    }

    test(
      "JointLedger.recover rebuilds Done from the fastBlockMark block brief + deposits, " +
          "co-anchoring the L2 ledger (the shared head/coil fast anchor)"
    ) {
        withStore { p =>
            for
                store <- InMemoryL2Store.create
                ledger <- EutxoL2Ledger(config, store)
                // Advance the live ledger past the crash boundary (to command number 3).
                _ <- (1 to 3).toList.traverse_(i =>
                    ledger.sendApplyDepositDecisions(noop(i)).value.flatMap(IO.fromEither)
                )
                // Crash boundary: fastBlockMark = block 2 (max BlockResult), recorded at L2 command
                // number 2. The header is read from the Block journal (present inbound on any peer),
                // so seed Block(2).
                brief <- blockBrief(2)
                _ <- p.put(JournalKey.Block(BlockNumber(2)))(JournalValue(stamp, brief))
                _ <- p.put(StoreKey.DepositMap)(DepositsMap.empty)
                _ <- p.put(StoreKey.L2CommandNumber(BlockNumber(2)))(L2CommandNumber(2L))
                recovered <- JointLedger.State.recover(p, ledger, Some(BlockNumber(2)))
                anchored <- ledger.currentCommandNumber
            yield assert(
              recovered.exists(d =>
                  d.previousBlockHeader == brief.header && d.deposits == DepositsMap.empty
              ) && anchored == L2CommandNumber(2L)
            )
        }
    }

    // ---- StackComposer ----

    test("StackComposer.recover returns None for an empty store (no own hard-ack)") {
        withStore { p =>
            StackComposer.State
                .recover(p, None, None, PeerId.Head(HeadPeerNumber(0)))
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
                us <- unsignedStack(stack = stackN, firstBlock = 4, lastBlock = lastBlock)
                _ <- p.put(JournalKey.HardAck(PeerId.Head(own), HardAckNumber(hardAckNum)))(
                  JournalValue(stamp, hardAck(peer = 0, ackNum = hardAckNum, stack = stackN))
                )
                _ <- p.put(StoreKey.UnsignedStack(StackNumber(stackN)))(us)
                _ <- p.put(StoreKey.Treasury)(TreasuryFixture.sampleTreasury)
                _ <- p.put(StoreKey.EvacuationMap(BlockNumber(lastBlock)))(EvacuationMap.empty)
                markers = Markers(
                  softConfirmed = None,
                  fastBlockMark = None,
                  hardConfirmed = Some(StackNumber(stackN)), // stack 2 hard-confirmed → gate armed
                  hardAcked = Some(HardAckNumber(hardAckNum)),
                  nextRequestNumber = RequestNumber(0)
                )
                recovered <- StackComposer.State.recover(
                  p,
                  markers.hardAcked,
                  markers.hardConfirmed,
                  PeerId.Head(own)
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
                us <- unsignedStack(stack = stackN, firstBlock = 0, lastBlock = lastBlock)
                _ <- p.put(JournalKey.HardAck(PeerId.Head(own), HardAckNumber(0)))(
                  JournalValue(stamp, hardAck(peer = 0, ackNum = 0, stack = stackN))
                )
                _ <- p.put(StoreKey.UnsignedStack(StackNumber(stackN)))(us)
                _ <- p.put(StoreKey.Treasury)(TreasuryFixture.sampleTreasury)
                _ <- p.put(StoreKey.EvacuationMap(BlockNumber(lastBlock)))(EvacuationMap.empty)
                // A stale BlockResult at the last closed block must be excluded (scan is exclusive).
                // Persisted BlockResults hold only the deltas; recover rehydrates each brief from the
                // Block journal, so seed Block(4)/Block(5) for the two scanned blocks.
                br3 <- blockResult(3)
                br4 <- blockResult(4)
                br5 <- blockResult(5)
                _ <- p.put(StoreKey.BlockResult(BlockNumber(3)))(br3.persisted)
                _ <- p.put(StoreKey.BlockResult(BlockNumber(4)))(br4.persisted)
                _ <- p.put(StoreKey.BlockResult(BlockNumber(5)))(br5.persisted)
                _ <- p.put(JournalKey.Block(BlockNumber(4)))(JournalValue(stamp, br4.brief))
                _ <- p.put(JournalKey.Block(BlockNumber(5)))(JournalValue(stamp, br5.brief))
                markers = Markers(
                  softConfirmed = None,
                  fastBlockMark = None,
                  hardConfirmed = Some(StackNumber(0)), // below stack 1 → gate disarmed
                  hardAcked = Some(HardAckNumber(0)),
                  nextRequestNumber = RequestNumber(0)
                )
                recovered <- StackComposer.State.recover(
                  p,
                  markers.hardAcked,
                  markers.hardConfirmed,
                  PeerId.Head(own)
                )
            yield assert(
              recovered.exists { s =>
                  s.pending.keySet == Set(BlockNumber(4), BlockNumber(5)) &&
                  !s.previousStackHardConfirmed
              }
            )
        }
    }

    test("StackComposer.recover (coil) returns None for an empty store (no own coil hard-ack)") {
        withStore { p =>
            StackComposer.State
                .recover(p, None, None, PeerId.Coil(CoilPeerNumber(0)))
                .map(r => assert(r.isEmpty))
        }
    }

    test(
      "StackComposer.recover (coil) rebuilds counters + snapshots from the last own coil HardAck; " +
          "lastBlockNum comes from the UnsignedStack, not a Stack journal"
    ) {
        withStore { p =>
            val coil = CoilPeerNumber(0)
            val hardAckNum = 5
            val stackN = 2
            val lastBlock = 7
            val sec = StandaloneEvacuationCommitment(
              blockNum = BlockNumber(lastBlock),
              blockVersion = BlockVersion.Full(0, 0),
              kzgCommitment = ByteString.fromArray(Array.fill[Byte](48)(0)),
              header = StandaloneEvacuationCommitment.Onchain.Serialized.fromBytes(
                Array.fill[Byte](32)(7)
              )
            )
            for
                sb <- stackBrief(stack = stackN, firstBlock = 4, lastBlock = lastBlock)
                // A coil peer has no own Stack journal; the closing stack's lastBlockNum comes from the
                // UnsignedStack it persists on every close.
                _ <- p.put(StoreKey.UnsignedStack(StackNumber(stackN)))(
                  Stack.Unsigned(
                    sb,
                    StackEffects.Unsigned.Regular(
                      NonEmptyList.one[PartitionEffects[StandaloneEvacuationCommitment]](
                        PartitionEffects.Minor(sec, List.empty)
                      )
                    )
                  )
                )
                // The last own hard-ack number is 5, belonging to stack 2 — the stack number comes
                // from the coil HardAck VALUE, read from the coil peer's own `HardAck` journal.
                _ <- p.put(JournalKey.HardAck(PeerId.Coil(coil), HardAckNumber(hardAckNum)))(
                  JournalValue(stamp, hardAck(peer = 0, ackNum = hardAckNum, stack = stackN))
                )
                _ <- p.put(StoreKey.Treasury)(TreasuryFixture.sampleTreasury)
                _ <- p.put(StoreKey.EvacuationMap(BlockNumber(lastBlock)))(EvacuationMap.empty)
                // A stale BlockResult at the last closed block is excluded (scan is exclusive).
                // Persisted BlockResults hold only the deltas; the brief is rehydrated from the
                // Block journal, so seed Block(8) for the one scanned block.
                br7 <- blockResult(7)
                br8 <- blockResult(8)
                _ <- p.put(StoreKey.BlockResult(BlockNumber(7)))(br7.persisted)
                _ <- p.put(StoreKey.BlockResult(BlockNumber(8)))(br8.persisted)
                _ <- p.put(JournalKey.Block(BlockNumber(8)))(JournalValue(stamp, br8.brief))
                recovered <- StackComposer.State.recover(
                  p,
                  Some(HardAckNumber(hardAckNum)),
                  Some(StackNumber(stackN)), // stack 2 hard-confirmed → gate armed
                  PeerId.Coil(coil)
                )
            yield assert(
              recovered.exists { s =>
                  s.lastClosedStackNum == StackNumber(stackN) &&
                  s.lastClosedBlockNum == BlockNumber(lastBlock) &&
                  s.nextOwnHardAckNum == HardAckNumber(hardAckNum + 1) &&
                  s.previousStackHardConfirmed &&
                  s.treasury == TreasuryFixture.sampleTreasury &&
                  s.evacuationMap == EvacuationMap.empty &&
                  s.pending.keySet == Set(BlockNumber(8))
              }
            )
        }
    }

    // ---- corruption (fail-safe) ----

    test(
      "JointLedger.recoverState throws on a non-empty store missing the fastBlockMark block brief"
    ) {
        withStore { p =>
            JointLedger.State
                .recoverState(p, Some(BlockNumber(2)))
                .attempt
                .map(r => assert(r.swap.toOption.exists(_.isInstanceOf[IllegalStateException])))
        }
    }

    test("JointLedger.recover throws when the fastBlockMark block's L2 command number is missing") {
        withStore { p =>
            for
                store <- InMemoryL2Store.create
                ledger <- EutxoL2Ledger(config, store)
                brief <- blockBrief(2)
                _ <- p.put(JournalKey.Block(BlockNumber(2)))(JournalValue(stamp, brief))
                _ <- p.put(StoreKey.DepositMap)(DepositsMap.empty)
                // L2CommandNumber intentionally not written
                r <- JointLedger.State.recover(p, ledger, Some(BlockNumber(2))).attempt
            yield assert(r.swap.toOption.exists(_.isInstanceOf[IllegalStateException]))
        }
    }

    test("StackComposer.recover throws on a non-empty store missing the Treasury snapshot") {
        withStore { p =>
            val own = HeadPeerNumber(0)
            for
                us <- unsignedStack(stack = 1, firstBlock = 0, lastBlock = 3)
                _ <- p.put(JournalKey.HardAck(PeerId.Head(own), HardAckNumber(0)))(
                  JournalValue(stamp, hardAck(peer = 0, ackNum = 0, stack = 1))
                )
                _ <- p.put(StoreKey.UnsignedStack(StackNumber(1)))(us)
                // Treasury intentionally not written
                markers = Markers(
                  None,
                  None,
                  Some(StackNumber(1)),
                  Some(HardAckNumber(0)),
                  RequestNumber(0)
                )
                r <- StackComposer.State
                    .recover(p, markers.hardAcked, markers.hardConfirmed, PeerId.Head(own))
                    .attempt
            yield assert(r.swap.toOption.exists(_.isInstanceOf[IllegalStateException]))
        }
    }

    // ---- UnsignedStack (SCA in-flight-cell recovery source) ----

    test("UnsignedStack round-trips a Regular stack's unsigned effects through the store") {
        withStore { p =>
            val key = StoreKey.UnsignedStack(StackNumber(1))
            val sec = StandaloneEvacuationCommitment(
              blockNum = BlockNumber(1),
              blockVersion = BlockVersion.Full(0, 0),
              kzgCommitment = ByteString.fromArray(Array.fill[Byte](48)(0)),
              header = StandaloneEvacuationCommitment.Onchain.Serialized.fromBytes(
                Array.fill[Byte](32)(7)
              )
            )
            for
                brief <- stackBrief(stack = 1, firstBlock = 0, lastBlock = 3)
                unsigned = Stack.Unsigned(
                  brief,
                  StackEffects.Unsigned.Regular(
                    NonEmptyList.one[PartitionEffects[StandaloneEvacuationCommitment]](
                      PartitionEffects.Minor(sec, List.empty)
                    )
                  )
                )
                _ <- p.put(key)(unsigned)
                got <- p.getOrFail(key)
            // The SEC header is an `IArray[Byte]` (reference equality), so compare the re-encoded
            // bytes rather than the case-class value: a faithful decode re-encodes identically.
            yield assert(key.encodeValue(unsigned) sameElements key.encodeValue(got))
        }
    }

    // ---- RequestSequencer (request counter) ----

    test("Markers.recoverNextRequestNumber returns RequestNumber(0) on an empty store") {
        withStore { p =>
            Markers
                .recoverNextRequestNumber(p.backend, HeadPeerNumber(0))
                .map(n => assert(n == RequestNumber(0)))
        }
    }

    test("Markers.recoverNextRequestNumber = max own Request + 1; other peers' requests ignored") {
        withStore { p =>
            val own = HeadPeerNumber(1)
            // Seed own requests (incl. one > 2^32 to exercise the 8-byte index decode), plus a
            // different peer at a higher number that must NOT raise own's counter (per-author CF
            // isolation — a different author is a different CF).
            for
                _ <- List(0L, 1L, 2L, 5_000_000_000L).traverse_(k =>
                    p.backend.put(
                      Cf.Request(own),
                      JournalKey.Request(own, RequestNumber(k)).encode,
                      Array[Byte](0)
                    )
                )
                _ <- p.backend.put(
                  Cf.Request(HeadPeerNumber(2)),
                  JournalKey.Request(HeadPeerNumber(2), RequestNumber(99L)).encode,
                  Array[Byte](0)
                )
                n <- Markers.recoverNextRequestNumber(p.backend, own)
            yield assert(n == RequestNumber(5_000_000_001L))
        }
    }

    // ---- CardanoLiaison (HardConfirmation fold) ----

    test("Markers.recoverFastBlockMark returns None for an empty store, else max(BlockResult)") {
        withStore { p =>
            for
                empty <- Markers.recoverFastBlockMark(p.backend)
                _ <- List(2, 5, 3).traverse_(n =>
                    blockResult(n).flatMap(br =>
                        p.put(StoreKey.BlockResult(BlockNumber(n)))(br.persisted)
                    )
                )
                mark <- Markers.recoverFastBlockMark(p.backend)
            yield assert(empty.isEmpty && mark.contains(BlockNumber(5)))
        }
    }

    test("HardConfirmationScan.scanFrom over an empty CF yields no entries") {
        withStore { p =>
            HardConfirmationScan
                .scanFrom(p.backend, StackNumber.zero)
                .map(entries => assert(entries.isEmpty))
        }
    }

    test("CardanoLiaison.recover over an empty HardConfirmation CF folds to State.empty") {
        // The effect-fold parity (recover == live path) rides on the kernels both call —
        // `State.applyInitialEffects`/`applyRegularEffects` — which the live `Stack.HardConfirmed`
        // path exercises (stage4); a fixture-level fold test is impractical here (the effect leaf
        // txs have no public constructors) and is left to the R4 crash tests.
        withStore { p =>
            CardanoLiaison.State
                .recover(p)
                .map(s => assert(s == CardanoLiaison.State.empty))
        }
    }

    // ---- LaneOutgoingBacking (the liaison outbound lanes' recover seam) ----

    test("LaneOutgoingBacking over an empty store has no high-water and an empty load") {
        withStore { p =>
            val hardAckBacking = LaneOutgoingBacking.hardAck(p.backend, PeerId.Head(ownNum))
            val blockBacking =
                LaneOutgoingBacking.block(p.backend, b => config.canLeadFast(b.blockNum))
            for
                hwHardAck <- hardAckBacking.highWater
                hwBlock <- blockBacking.highWater
                loaded <- hardAckBacking.backfill(HardAckNumber.zero, 16)
            yield assert(hwHardAck.isEmpty && hwBlock.isEmpty && loaded.isEmpty)
        }
    }

    test(
      "LaneOutgoingBacking high-water = max key; load filters spines to own-led, satellites are per-author"
    ) {
        withStore { p =>
            val other = HeadPeerNumber(1)
            val spineRange = (0 to 5).toList
            val hardAckBacking = LaneOutgoingBacking.hardAck(p.backend, PeerId.Head(ownNum))
            val blockBacking =
                LaneOutgoingBacking.block(p.backend, b => config.canLeadFast(b.blockNum))
            val stackBacking =
                LaneOutgoingBacking.stack(p.backend, s => config.canLeadSlow(s.stackNum))
            for
                // own HardAcks 0..2 + another peer's at 5; the per-author CF excludes the other's.
                _ <- List(0, 1, 2).traverse_(k =>
                    p.put(JournalKey.HardAck(PeerId.Head(ownNum), HardAckNumber(k)))(
                      JournalValue(stamp, hardAck(peer = ownNum.convert, ackNum = k, stack = 0))
                    )
                )
                _ <- p.put(JournalKey.HardAck(PeerId.Head(other), HardAckNumber(5)))(
                  JournalValue(stamp, hardAck(peer = other.convert, ackNum = 5, stack = 0))
                )
                // Block + Stack spines carry every leader's brief; load keeps only own-led.
                _ <- spineRange.traverse_(n =>
                    blockBrief(n).flatMap(b =>
                        p.put(JournalKey.Block(BlockNumber(n)))(JournalValue(stamp, b))
                    )
                )
                _ <- spineRange.traverse_(n =>
                    stackBrief(stack = n, firstBlock = 0, lastBlock = n).flatMap(s =>
                        p.put(JournalKey.Stack(StackNumber(n)))(JournalValue(stamp, s))
                    )
                )
                hwHardAck <- hardAckBacking.highWater
                hwBlock <- blockBacking.highWater
                hwStack <- stackBacking.highWater
                ownHardAcks <- hardAckBacking.backfill(HardAckNumber.zero, 16)
                ownBlocks <- blockBacking.backfill(BlockNumber.zero, 16)
                ownStacks <- stackBacking.backfill(StackNumber.zero, 16)
            yield assert(
              // satellite high-water = own max (other peer's CF is separate);
              hwHardAck == Some(HardAckNumber(2)) &&
                  ownHardAcks.map(_.hardAckNum) == List(0, 1, 2).map(HardAckNumber(_)) &&
                  // spine high-water = overall max (no filter); load keeps only own-led.
                  hwBlock == Some(BlockNumber(5)) && hwStack == Some(StackNumber(5)) &&
                  ownBlocks.map(_.blockNum) ==
                  spineRange.map(BlockNumber(_)).filter(config.canLeadFast) &&
                  ownStacks.map(_.stackNum) ==
                  spineRange.map(StackNumber(_)).filter(config.canLeadSlow)
            )
        }
    }

    // ---- fixtures ----

    private def noop(n: Int): L2LedgerCommand.ApplyDepositDecisions =
        L2LedgerCommand.ApplyDepositDecisions(
          blockNumber = BlockNumber(n),
          blockCreationEndTime = BigInt(n),
          absorbedDeposits = Nil,
          rejectedDeposits = Nil
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
          BlockBody.Minor(requests = List.empty, depositsRejected = List.empty)
        )
    }

    private def blockResult(blockNum: Int): IO[BlockResult] =
        realTimeQuantizedInstant(headConfig.slotConfig).map { now =>
            val end = BlockCreationEndTime(now + 1.second)
            BlockResult(
              brief = minorBrief(blockNum, now),
              evacuationMapDiff = Nil,
              payoutObligations = Nil,
              payoutRequestIds = Nil,
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

    /** The `Stack.Unsigned` a peer persists on every close — recover reads `brief.lastBlockNum`
      * from it (the unified §6 source for both peer types). Simplest Regular shape: one Minor SEC
      * at the stack's last block.
      */
    private def unsignedStack(stack: Int, firstBlock: Int, lastBlock: Int): IO[Stack.Unsigned] =
        stackBrief(stack, firstBlock, lastBlock).map { sb =>
            val sec = StandaloneEvacuationCommitment(
              blockNum = BlockNumber(lastBlock),
              blockVersion = BlockVersion.Full(0, 0),
              kzgCommitment = ByteString.fromArray(Array.fill[Byte](48)(0)),
              header = StandaloneEvacuationCommitment.Onchain.Serialized.fromBytes(
                Array.fill[Byte](32)(7)
              )
            )
            Stack.Unsigned(
              sb,
              StackEffects.Unsigned.Regular(
                NonEmptyList.one[PartitionEffects[StandaloneEvacuationCommitment]](
                  PartitionEffects.Minor(sec, List.empty)
                )
              )
            )
        }

    /** A hard-ack with a chosen `stackNum` — payload is the simplest round-2 shape (recover reads
      * only `stackNum`).
      */
    private def hardAck(peer: Int, ackNum: Int, stack: Int): HardAck =
        HardAck(
          ackId = HardAckId(PeerId.Head(HeadPeerNumber(peer)), HardAckNumber(ackNum)),
          stackNum = StackNumber(stack),
          payload = HardAck.Round2Payload.Regular(
            TxSignature(IArray.from(Array.fill[Byte](64)(0)))
          )
        )

    private def withStore(prog: Persistence[IO] => IO[Assertion]): Assertion =
        val persistenceTracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
        InMemoryBackendStore
            .open(persistenceTracer)
            .use(backend => Persistence.fromBackend(backend, persistenceTracer).flatMap(prog))
            .unsafeRunSync()
