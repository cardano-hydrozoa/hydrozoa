package hydrozoa.multisig.consensus.liaison

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.multisig.timing.TxTiming.StackTimes.StackCreationEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber, HardAckWithId, HubHardAckNumber}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.{BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}
import hydrozoa.multisig.persistence.recovery.OutboxBacking
import hydrozoa.multisig.persistence.{ArrivalStamp, InMemoryBackendStore, FamilyKey, FamilyValue, Persistence}
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt

/** Recovery tests for the coil-liaison outbound lanes' backing ([[OutboxBacking]], §6 PeerLiaison,
  * GUM-153). After a crash each lane restores only its high-water and serves older entries from the
  * store on demand (`load`), rather than eagerly loading its whole own production:
  *
  *   - a coil peer's own-hard-ack lane is backed by its own `CoilHardAck` family;
  *   - a hub's full-population lanes are backed by the families it already holds — every block (no
  *     `canLead` filter on a hub→coil link), every author's acks, every hub's relay lane.
  */
class PeerLiaisonCoilRecoveryTest extends AnyFunSuite:

    private val config: NodeConfig =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(0L))
    private val headConfig: HeadConfig = config.headConfig
    private given CardanoNetwork.Section = config
    private val stamp: ArrivalStamp = ArrivalStamp(generation = 0, monotonicNanos = 1L)

    test("coil own-hard-ack backing: high-water = max, load returns the tail from a number") {
        val coil = CoilPeerNumber(0)
        val (hw, fromZero, fromOne) = run { p =>
            val backing = OutboxBacking.coilHardAck(p.backend, coil)
            for {
                _ <- p.put(FamilyKey.CoilHardAck(coil, HardAckNumber(0)))(
                  FamilyValue(stamp, coilHardAck(coil, 0, stack = 1))
                )
                _ <- p.put(FamilyKey.CoilHardAck(coil, HardAckNumber(1)))(
                  FamilyValue(stamp, coilHardAck(coil, 1, stack = 2))
                )
                hw <- backing.highWater
                fromZero <- backing.backfill(HardAckNumber(0), 16)
                fromOne <- backing.backfill(HardAckNumber(1), 16)
            } yield (hw, fromZero, fromOne)
        }
        assert(hw == Some(HardAckNumber(1)), s"high-water = max; got $hw")
        assert(fromZero.map(_.hardAckNum) == List(HardAckNumber(0), HardAckNumber(1)), s"$fromZero")
        assert(fromOne.map(_.hardAckNum) == List(HardAckNumber(1)), s"load from 1 → tail; $fromOne")
    }

    test("coil own-hard-ack backing on an empty store: no high-water, empty load") {
        val (hw, loaded) = run { p =>
            val backing = OutboxBacking.coilHardAck(p.backend, CoilPeerNumber(0))
            for {
                hw <- backing.highWater
                loaded <- backing.backfill(HardAckNumber.zero, 16)
            } yield (hw, loaded)
        }
        assert(hw.isEmpty && loaded.isEmpty)
    }

    test("hub population backings: high-water + load over blocks (no filter), acks, relay lane") {
        val h0 = HeadPeerNumber(0)
        val hub0 = HeadPeerNumber(0)
        case class Out(
            blockHw: Option[BlockNumber],
            blocks: List[BlockNumber],
            stacks: List[StackNumber],
            headAcks: List[HardAckNumber],
            relay: List[HubHardAckNumber]
        )
        val out = run { p =>
            val blockBacking = OutboxBacking.block(p.backend, _ => true)
            val stackBacking = OutboxBacking.stack(p.backend, _ => true)
            val headHardAckBacking = OutboxBacking.hardAck(p.backend, h0)
            val relayBacking = OutboxBacking.hubHardAck(p.backend, hub0)
            for {
                // Two blocks on the spine — a hub→coil link keeps BOTH (no canLead filter).
                b1 <- blockBrief(1)
                b2 <- blockBrief(2)
                _ <- p.put(FamilyKey.Block(BlockNumber(1)))(FamilyValue(stamp, b1))
                _ <- p.put(FamilyKey.Block(BlockNumber(2)))(FamilyValue(stamp, b2))
                s1 <- stackBrief(1, 1, 2)
                _ <- p.put(FamilyKey.Stack(StackNumber(1)))(FamilyValue(stamp, s1))
                _ <- p.put(FamilyKey.HardAck(h0, HardAckNumber(0)))(
                  FamilyValue(stamp, headHardAck(h0, 0, stack = 1))
                )
                _ <- p.put(FamilyKey.HubHardAck(hub0, HubHardAckNumber(0)))(
                  FamilyValue(
                    stamp,
                    HardAckWithId(hub0, HubHardAckNumber(0), coilHardAck(CoilPeerNumber(0), 0, 1))
                  )
                )
                blockHw <- blockBacking.highWater
                blocks <- blockBacking.backfill(BlockNumber(1), 16)
                stacks <- stackBacking.backfill(StackNumber(1), 16)
                headAcks <- headHardAckBacking.backfill(HardAckNumber.zero, 16)
                relay <- relayBacking.backfill(HubHardAckNumber.zero, 16)
            } yield Out(
              blockHw,
              blocks.map(_.blockNum),
              stacks.map(_.stackNum),
              headAcks.map(_.hardAckNum),
              relay.map(_.seqNum)
            )
        }
        assert(out.blockHw == Some(BlockNumber(2)), "block high-water = max")
        assert(out.blocks == List(BlockNumber(1), BlockNumber(2)), "all blocks, no canLead filter")
        assert(out.stacks == List(StackNumber(1)), "the stack")
        assert(out.headAcks == List(HardAckNumber(0)), "head hard-ack")
        assert(out.relay == List(HubHardAckNumber(0)), "hub relay lane")
    }

    /** Open a fresh in-memory store, then run `body` against a `Persistence` over it. */
    private def run[A](body: Persistence[IO] => IO[A]): A =
        InMemoryBackendStore.open
            .use(backend => Persistence.fromBackend(backend).flatMap(body))
            .unsafeRunSync()

    private def coilHardAck(coil: CoilPeerNumber, hardAckNum: Int, stack: Int): HardAck =
        HardAck(
          ackId = HardAckId(PeerId.Coil(coil), HardAckNumber(hardAckNum)),
          stackNum = StackNumber(stack),
          payload = HardAck.Round2Payload.Regular(TxSignature(IArray.from(Array.fill[Byte](64)(0))))
        )

    private def headHardAck(peer: HeadPeerNumber, hardAckNum: Int, stack: Int): HardAck =
        HardAck(
          ackId = HardAckId(PeerId.Head(peer), HardAckNumber(hardAckNum)),
          stackNum = StackNumber(stack),
          payload = HardAck.Round2Payload.Regular(TxSignature(IArray.from(Array.fill[Byte](64)(0))))
        )

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
