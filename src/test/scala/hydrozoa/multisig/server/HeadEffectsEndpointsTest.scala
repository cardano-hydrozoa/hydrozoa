package hydrozoa.multisig.server

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.multisig.timing.TxTiming.StackTimes.StackCreationEndTime
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.NodeStatus
import hydrozoa.multisig.consensus.UserRequest.TransactionRequest
import hydrozoa.multisig.consensus.UserRequestBody.TransactionRequestBody
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{BlockWeaver, RequestSequencer, UserRequestWithId}
import hydrozoa.multisig.ledger.block.{Block, BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.stack.{EffectIds, PartitionEffects, StackBrief, StackEffects, StackNumber, StandaloneEvacuationCommitment}
import hydrozoa.multisig.persistence.{ArrivalStamp, ConsensusStoreReader, RequestBlockEntry, Timestamped}
import hydrozoa.rulebased.ledger.l1.state.StandaloneEvacuationCommitmentOnchain
import io.circe.Json
import java.time.Instant
import org.http4s.circe.*
import org.http4s.implicits.*
import org.http4s.{HttpApp, Method, Request, Status, Uri}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.TransactionHash
import scalus.uplc.builtin.ByteString

/** The block-effects queries through the HTTP layer against a stubbed [[ConsensusStoreReader]],
  * exercising a single-minor stack whose only effect is a standalone evacuation commitment (SEC):
  * the by-kind listing, the SEC sub-resource (head/coil signature split, synthetic l1TxId), the
  * `GET /head/effects/<l1TxId>` reverse lookup, and the 404 / 400 edges. The real-tx effect kinds
  * (settlement, rollout, finalization, refund) are exercised end-to-end by the stage suites.
  */
class HeadEffectsEndpointsTest extends AnyFunSuite:

    private val multiNodeConfig: MultiNodeConfig =
        MultiNodeConfig.generateDefault.pureApply(Gen.Parameters.default, Seed(0L))
    private val headConfig = multiNodeConfig.headConfig
    private val nHeadPeers = headConfig.nHeadPeers.convert

    /** A minor brief for block 1. */
    private def mkMinorBrief1: IO[BlockBrief.Minor] =
        for now <- realTimeQuantizedInstant(headConfig.slotConfig)
        yield {
            val startTime = BlockCreationStartTime(now)
            val endTime = BlockCreationEndTime(now + 1.second)
            val fallbackTxStartTime = headConfig.txTiming.newFallbackStartTime(endTime)
            BlockBrief.Minor(
              BlockHeader.Minor(
                blockNum = BlockNumber(1),
                blockVersion = BlockVersion.Full(0, 0),
                startTime = startTime,
                endTime = endTime,
                fallbackTxStartTime = fallbackTxStartTime,
                forcedMajorBlockWakeupTime =
                    headConfig.txTiming.forcedMajorBlockWakeupTime(fallbackTxStartTime),
                mDepositDecisionWakeupTime = None
              ),
              BlockBody.Minor(events = List.empty, depositsRefunded = List.empty)
            )
        }

    /** A hard-confirmed SEC committing block 1, with `nHeadPeers + 1` header signatures (so the
      * head/coil split has a coil signature on the tail).
      */
    private val sec: StandaloneEvacuationCommitment.MultiSigned =
        StandaloneEvacuationCommitment.MultiSigned(
          commitment = StandaloneEvacuationCommitment(
            blockNum = BlockNumber(1),
            blockVersion = BlockVersion.Full(1, 0),
            kzgCommitment = EvacuationMap.empty.kzgCommitment,
            header = StandaloneEvacuationCommitmentOnchain(
              StandaloneEvacuationCommitmentOnchain(
                headId = headConfig.headTokenNames.treasuryTokenName.bytes,
                versionMajor = BigInt(1),
                versionMinor = BigInt(0),
                commitment = EvacuationMap.empty.kzgCommitment
              )
            )
          ),
          headerMultiSigned = List.tabulate(nHeadPeers + 1)(i =>
              BlockHeader.Minor.HeaderSignature(IArray(i.toByte, (i + 1).toByte))
          )
        )

    private val secId: TransactionHash = EffectIds.secL1TxId(sec.commitment)

    /** A transaction request processed in block 1 — its related effect is that block's SEC. */
    private val txRequestId: RequestId = RequestId(HeadPeerNumber(0), RequestNumber(0))
    private val txRequest: UserRequestWithId =
        UserRequestWithId(TransactionRequest(TransactionRequestBody(ByteString.empty)), txRequestId)

    private val stackEffects: StackEffects.HardConfirmed =
        StackEffects.HardConfirmed.Regular(
          NonEmptyList.of(PartitionEffects.Minor(sec = sec, refunds = List.empty))
        )

    /** A stub reader over a single-minor stack (stack 1 = block 1) carrying just the SEC. */
    private def stubReader(brief: BlockBrief.Minor): ConsensusStoreReader[IO] =
        new ConsensusStoreReader[IO]:
            def blockBriefs: IO[List[BlockBrief.Next]] = IO.pure(List(brief))
            def blockBrief(num: BlockNumber): IO[Option[BlockBrief.Next]] =
                IO.pure(Option.when(num == BlockNumber(1))(brief))
            def softConfirmation(
                num: BlockNumber
            ): IO[Option[Timestamped[Block.SoftConfirmed.Next]]] = IO.pure(None)
            def stackOf(num: BlockNumber): IO[Option[StackNumber]] =
                IO.pure(Option.when(num == BlockNumber(1))(StackNumber(1)))
            def hardConfirmation(
                num: StackNumber
            ): IO[Option[Timestamped[StackEffects.HardConfirmed]]] =
                IO.pure(
                  Option.when(num == StackNumber(1))(
                    Timestamped(ArrivalStamp(0, 0L), stackEffects)
                  )
                )
            def stackBrief(num: StackNumber): IO[Option[StackBrief]] =
                IO.pure(
                  Option.when(num == StackNumber(1))(
                    StackBrief(
                      StackNumber(1),
                      BlockNumber(1),
                      BlockNumber(1),
                      StackCreationEndTime(
                        QuantizedInstant.ofEpochSeconds(headConfig.slotConfig, 0L)
                      )
                    )
                  )
                )
            def effectStack(l1TxId: TransactionHash): IO[Option[StackNumber]] =
                IO.pure(Option.when(l1TxId == secId)(StackNumber(1)))
            def requestsOf(peer: HeadPeerNumber): IO[List[Timestamped[UserRequestWithId]]] =
                IO.pure(Nil)
            def request(id: RequestId): IO[Option[Timestamped[UserRequestWithId]]] =
                IO.pure(
                  Option.when(id == txRequestId)(Timestamped(ArrivalStamp(0, 0L), txRequest))
                )
            def requestBlock(id: RequestId): IO[Option[RequestBlockEntry]] =
                IO.pure(
                  Option.when(id == txRequestId)(
                    RequestBlockEntry(BlockNumber(1), ValidityFlag.Valid)
                  )
                )
            def absorptionBlock(id: RequestId): IO[Option[BlockNumber]] = IO.pure(None)
            def wallClockOf(stamp: ArrivalStamp): IO[Option[Instant]] = IO.pure(None)

    private def withRoutes(check: HttpApp[IO] => IO[Unit]): Unit =
        mkMinorBrief1
            .flatMap(brief =>
                ActorSystem[IO]("HeadEffectsEndpointsTest").use { system =>
                    for {
                        reqStub <- system.actorOf(new Actor[IO, RequestSequencer.Request] {
                            override def receive: Receive[IO, RequestSequencer.Request] =
                                _ => IO.pure(())
                        })
                        bwStub <- system.actorOf(new Actor[IO, BlockWeaver.Request] {
                            override def receive: Receive[IO, BlockWeaver.Request] =
                                _ => IO.pure(())
                        })
                        routes <- HydrozoaRoutes(
                          reqStub,
                          bwStub,
                          IO.pure(NodeStatus.Active),
                          stubReader(brief),
                          None,
                          headConfig,
                          HydrozoaServer.Config(adminUsername = "admin", adminPassword = "admin"),
                          ContraTracer[IO, HydrozoaHttpEvent](_ => IO.unit)
                        )
                        _ <- check(routes.routes.orNotFound)
                    } yield ()
                }
            )
            .unsafeRunSync()

    private def get(app: HttpApp[IO], path: String): IO[(Status, Json)] =
        for {
            resp <- app.run(Request[IO](Method.GET, Uri.unsafeFromString(path)))
            body <- resp.as[Json]
        } yield (resp.status, body)

    test("GET /head/blocks/1/effects lists the block's SEC by kind, no other effects") {
        withRoutes { app =>
            get(app, "/head/blocks/1/effects").map { (status, body) =>
                val _ = assert(status == Status.Ok)
                val c = body.hcursor
                val _ = assert(c.get[String]("blockType") == Right("minor"))
                val _ = assert(c.get[String]("sec") == Right(secId.toHex))
                val _ = assert(c.get[List[String]]("refunds") == Right(Nil))
                val _ = assert(c.get[List[String]]("rollouts") == Right(Nil))
                val _ = assert(c.downField("settlement").focus.forall(_.isNull))
                ()
            }
        }
    }

    test("GET /head/blocks/1/effects/sec returns the SEC in full with the head/coil split") {
        withRoutes { app =>
            get(app, "/head/blocks/1/effects/sec").map { (status, body) =>
                val _ = assert(status == Status.Ok)
                val c = body.hcursor
                val _ = assert(c.get[String]("l1TxId") == Right(secId.toHex))
                val _ = assert(c.get[String]("kind") == Right("sec"))
                val _ = assert(c.get[Int]("blockNumber") == Right(1))
                val _ = assert(c.downField("secOnchainSerialized").as[String].isRight)
                // nHeadPeers head signatures, the remaining tail as coil signatures.
                val _ = assert(c.get[List[String]]("headSignatures").exists(_.size == nHeadPeers))
                val _ = assert(c.get[List[String]]("coilSignatures").exists(_.size == 1))
                ()
            }
        }
    }

    test("GET /head/effects/<secId> resolves the same SEC via the reverse index") {
        withRoutes { app =>
            get(app, s"/head/effects/${secId.toHex}").map { (status, body) =>
                val _ = assert(status == Status.Ok)
                val _ = assert(body.hcursor.get[String]("l1TxId") == Right(secId.toHex))
                val _ = assert(body.hcursor.get[String]("kind") == Right("sec"))
                ()
            }
        }
    }

    test("GET /head/requests/<id> carries the transaction's related effect (its block's SEC)") {
        withRoutes { app =>
            get(app, s"/head/requests/${txRequestId.asI64}").map { (status, body) =>
                val _ = assert(status == Status.Ok)
                val effects = body.hcursor.downField("status").downField("relatedEffects")
                val _ = assert(effects.downN(0).get[String]("l1TxId") == Right(secId.toHex))
                val _ = assert(effects.downN(0).get[String]("kind") == Right("sec"))
                ()
            }
        }
    }

    test("absent effect kinds, unknown ids, and malformed ids are 404 / 400, not 500") {
        withRoutes { app =>
            for {
                noSettlement <- get(app, "/head/blocks/1/effects/settlement")
                missingBlock <- get(app, "/head/blocks/9/effects")
                unknownId <- app.run(
                  Request[IO](Method.GET, Uri.unsafeFromString(s"/head/effects/${"ab" * 32}"))
                )
                malformedId <- app.run(
                  Request[IO](Method.GET, Uri.unsafeFromString("/head/effects/not-hex"))
                )
            } yield {
                val _ = assert(noSettlement._1 == Status.NotFound)
                val _ = assert(missingBlock._1 == Status.NotFound)
                val _ = assert(unknownId.status == Status.NotFound)
                val _ = assert(malformedId.status.code >= 400 && malformedId.status.code < 500)
                ()
            }
        }
    }
