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
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.liaison.PeerLiaisonHeadToHead
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
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
import hydrozoa.multisig.persistence.{ArrivalStamp, Cf, InMemoryBackendStore, LaneKey, LaneValue, Markers, Persistence, StoreKey}
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt
import scalus.uplc.builtin.ByteString

/** Tests for the R2-fast recover seams — the pure-over-store reconstruction of `JointLedger`'s and
  * `StackComposer`'s passive state after a crash. Each test seeds a store with the typed values the
  * producers write (`persistOwnAckBundle` / `persistOwnStackClose` /
  * `PeerLiaisonHeadToHead.persistInbound`) and asserts the recovered state equals the crash-time
  * value. Markers / `softAcked` are passed directly — the marker-derivation → recover wiring is R3.
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

    test("JointLedger.recoverCoil returns None for an empty store (no BlockResult)") {
        withStore { p =>
            for
                store <- InMemoryL2Store.create
                ledger <- EutxoL2Ledger(config, store)
                recovered <- JointLedger.State.recoverCoil(p, ledger, None)
            yield assert(recovered.isEmpty)
        }
    }

    test(
      "JointLedger.recoverCoil rebuilds Done from the coilBlockMark BlockResult + deposits, " +
          "co-anchoring the L2 ledger"
    ) {
        withStore { p =>
            for
                store <- InMemoryL2Store.create
                ledger <- EutxoL2Ledger(config, store)
                // Advance the live ledger past the crash boundary (to command number 3).
                _ <- (1 to 3).toList.traverse_(i =>
                    ledger.sendApplyDepositDecisions(noop(i)).value.flatMap(IO.fromEither)
                )
                // Coil crash boundary: coilBlockMark = block 2, recorded at L2 command number 2. A
                // coil peer writes no own SoftAck/Block lane, so its header comes from BlockResult.
                br <- blockResult(2)
                _ <- p.put(StoreKey.BlockResult(BlockNumber(2)))(br)
                _ <- p.put(StoreKey.DepositMap)(DepositsMap.empty)
                _ <- p.put(StoreKey.L2CommandNumber(BlockNumber(2)))(L2CommandNumber(2L))
                recovered <- JointLedger.State.recoverCoil(p, ledger, Some(BlockNumber(2)))
                anchored <- ledger.currentCommandNumber
            yield assert(
              recovered.exists(d =>
                  d.previousBlockHeader == br.brief.header && d.deposits == DepositsMap.empty
              ) && anchored == L2CommandNumber(2L)
            )
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
                      LaneKey.Request(own, RequestNumber(k)).encode,
                      Array[Byte](0)
                    )
                )
                _ <- p.backend.put(
                  Cf.Request(HeadPeerNumber(2)),
                  LaneKey.Request(HeadPeerNumber(2), RequestNumber(99L)).encode,
                  Array[Byte](0)
                )
                n <- Markers.recoverNextRequestNumber(p.backend, own)
            yield assert(n == RequestNumber(5_000_000_001L))
        }
    }

    // ---- CardanoLiaison (HardConfirmation fold) ----

    test("Markers.recoverCoilBlockMark returns None for an empty store, else max(BlockResult)") {
        withStore { p =>
            for
                empty <- Markers.recoverCoilBlockMark(p.backend)
                _ <- List(2, 5, 3).traverse_(n =>
                    blockResult(n).flatMap(br => p.put(StoreKey.BlockResult(BlockNumber(n)))(br))
                )
                mark <- Markers.recoverCoilBlockMark(p.backend)
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

    test("CardanoLiaison.recover over an empty HardConfirmation CF folds to initialState") {
        // The effect-fold parity (recover == live path) rides on the kernels both call —
        // `State.applyInitialEffects`/`applyRegularEffects` — which the live `Stack.HardConfirmed`
        // path exercises (stage4); a fixture-level fold test is impractical here (the effect leaf
        // txs have no public constructors) and is left to the R4 crash tests.
        withStore { p =>
            CardanoLiaison.State
                .recover(p, config)
                .map(s => assert(s == CardanoLiaison.State.initialState(config)))
        }
    }

    // ---- PeerLiaisonHeadToHead (outbox restore) ----

    test("PeerLiaisonHeadToHead.recover over an empty store yields an empty outbox") {
        withStore { p =>
            PeerLiaisonHeadToHead
                .recover(p, ownNum, config.canLeadFast, config.canLeadSlow)
                .map { seed =>
                    assert(
                      seed.softAcks.isEmpty && seed.blocks.isEmpty && seed.requests.isEmpty &&
                          seed.stacks.isEmpty && seed.hardAcks.isEmpty
                    )
                }
        }
    }

    test(
      "PeerLiaisonHeadToHead.recover restores own satellites (per-peer) and own-led spines " +
          "(filtered)"
    ) {
        withStore { p =>
            val other = HeadPeerNumber(1)
            val spineRange = (0 to 5).toList
            for
                // own HardAcks 0..2 + another peer's at 5 (must be excluded by the per-peer scan).
                _ <- List(0, 1, 2).traverse_(k =>
                    p.put(LaneKey.HardAck(ownNum, HardAckNumber(k)))(
                      LaneValue(stamp, hardAck(peer = ownNum.convert, ackNum = k, stack = 0))
                    )
                )
                _ <- p.put(LaneKey.HardAck(other, HardAckNumber(5)))(
                  LaneValue(stamp, hardAck(peer = other.convert, ackNum = 5, stack = 0))
                )
                // Block + Stack spines carry every leader's brief; recover keeps only own-led.
                _ <- spineRange.traverse_(n =>
                    blockBrief(n).flatMap(b =>
                        p.put(LaneKey.Block(BlockNumber(n)))(LaneValue(stamp, b))
                    )
                )
                _ <- spineRange.traverse_(n =>
                    stackBrief(stack = n, firstBlock = 0, lastBlock = n).flatMap(s =>
                        p.put(LaneKey.Stack(StackNumber(n)))(LaneValue(stamp, s))
                    )
                )
                seed <- PeerLiaisonHeadToHead
                    .recover(p, ownNum, config.canLeadFast, config.canLeadSlow)
            yield assert(
              seed.hardAcks.map(_.hardAckNum) == List(0, 1, 2).map(HardAckNumber(_)) &&
                  seed.blocks.map(_.blockNum) ==
                  spineRange.map(BlockNumber(_)).filter(config.canLeadFast) &&
                  seed.stacks.map(_.stackNum) ==
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
          ackId = HardAckId(PeerId.Head(HeadPeerNumber(peer)), HardAckNumber(ackNum)),
          stackNum = StackNumber(stack),
          payload = HardAck.Round2Payload.Regular(
            TxSignature(IArray.from(Array.fill[Byte](64)(0)))
          )
        )

    private def withStore(prog: Persistence[IO] => IO[Assertion]): Assertion =
        InMemoryBackendStore.open
            .use(backend => Persistence.fromBackend(backend).flatMap(prog))
            .unsafeRunSync()
