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
import hydrozoa.multisig.persistence.{ArrivalStamp, InMemoryBackendStore, LaneKey, LaneValue, Persistence}
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt

/** Recovery tests for the two coil-liaison outbox recovers (§6 PeerLiaison, GUM-153):
  *
  *   - [[PeerLiaisonCoilToHub.recover]] rebuilds a coil peer's own-hard-ack outbox from its own
  *     `CoilHardAck` family;
  *   - [[PeerLiaisonHubToCoil.recover]] rebuilds a hub's full-population outbox from the population
  *     families it already holds — every block (no `canLead` filter), every author's acks, every
  *     hub's relay lane.
  */
class PeerLiaisonCoilRecoveryTest extends AnyFunSuite:

    private val config: NodeConfig =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(0L))
    private val headConfig: HeadConfig = config.headConfig
    private given CardanoNetwork.Section = config
    private val stamp: ArrivalStamp = ArrivalStamp(generation = 0, monotonicNanos = 1L)

    test("CoilToHub.recover rebuilds the coil peer's own-hard-ack outbox from CoilHardAck") {
        val coil = CoilPeerNumber(0)
        val acks = run { p =>
            for {
                _ <- p.put(LaneKey.CoilHardAck(coil, HardAckNumber(0)))(
                  LaneValue(stamp, coilHardAck(coil, 0, stack = 1))
                )
                _ <- p.put(LaneKey.CoilHardAck(coil, HardAckNumber(1)))(
                  LaneValue(stamp, coilHardAck(coil, 1, stack = 2))
                )
            } yield ()
        }(PeerLiaisonCoilToHub.recover(_, coil))
        assert(acks.map(_.hardAckNum) == List(HardAckNumber(0), HardAckNumber(1)), s"got $acks")
    }

    test("CoilToHub.recover on an empty store seeds nothing") {
        val acks = run(_ => IO.unit)(PeerLiaisonCoilToHub.recover(_, CoilPeerNumber(0)))
        assert(acks.isEmpty)
    }

    test("HubToCoil.recover rebuilds the full population (all blocks, per-author + per-hub acks)") {
        val h0 = HeadPeerNumber(0)
        val hub0 = HeadPeerNumber(0)
        val seed = run { p =>
            for {
                // Two blocks on the spine — recover keeps BOTH regardless of leader (no canLead).
                b1 <- blockBrief(1)
                b2 <- blockBrief(2)
                _ <- p.put(LaneKey.Block(BlockNumber(1)))(LaneValue(stamp, b1))
                _ <- p.put(LaneKey.Block(BlockNumber(2)))(LaneValue(stamp, b2))
                s1 <- stackBrief(1, 1, 2)
                _ <- p.put(LaneKey.Stack(StackNumber(1)))(LaneValue(stamp, s1))
                // A head peer's hard-acks + a hub's relay lane.
                _ <- p.put(LaneKey.HardAck(h0, HardAckNumber(0)))(
                  LaneValue(stamp, headHardAck(h0, 0, stack = 1))
                )
                _ <- p.put(LaneKey.HubHardAck(hub0, HubHardAckNumber(0)))(
                  LaneValue(
                    stamp,
                    HardAckWithId(hub0, HubHardAckNumber(0), coilHardAck(CoilPeerNumber(0), 0, 1))
                  )
                )
            } yield ()
        }(PeerLiaisonHubToCoil.recover(_, headPeerNums = List(h0), hubNums = List(hub0)))

        assert(
          seed.blocks.map(_.blockNum) == List(BlockNumber(1), BlockNumber(2)),
          "all blocks, no canLead filter"
        )
        assert(seed.stacks.map(_.stackNum) == List(StackNumber(1)), "the stack")
        assert(
          seed.headHardAcks.getOrElse(h0, Nil).map(_.hardAckNum) == List(HardAckNumber(0)),
          "head hard-ack"
        )
        assert(
          seed.coilHardAcks.getOrElse(hub0, Nil).map(_.seqNum) == List(HubHardAckNumber(0)),
          "hub relay lane"
        )
        assert(
          seed.requests.getOrElse(h0, Nil).isEmpty && seed.softAcks.getOrElse(h0, Nil).isEmpty,
          "unseeded families empty"
        )
    }

    /** Open a fresh in-memory store, seed it, and run a recover against it. */
    private def run[A](seed: Persistence[IO] => IO[Unit])(recover: Persistence[IO] => IO[A]): A =
        InMemoryBackendStore.open
            .use(backend =>
                for {
                    p <- Persistence.fromBackend(backend)
                    _ <- seed(p)
                    out <- recover(p)
                } yield out
            )
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
