package hydrozoa.multisig.server

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.NodeStatus
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{BlockWeaver, RequestSequencer, UserRequestWithId}
import hydrozoa.multisig.ledger.block.{Block, BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StackBrief, StackEffects, StackNumber, StandaloneEvacuationCommitment}
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

/** The `/head/blocks` queries through the HTTP layer, against a stubbed [[ConsensusStoreReader]]:
  * the listing (block 0 synthesized from config + the spine briefs), the details' confirmation
  * ladder (PROPOSED → SOFT → HARD with the node-local times), the header content, and the 404/400
  * edges.
  */
class HeadBlocksEndpointsTest extends AnyFunSuite:

    private val multiNodeConfig: MultiNodeConfig =
        MultiNodeConfig.generateDefault.pureApply(Gen.Parameters.default, Seed(0L))
    private val headConfig = multiNodeConfig.headConfig

    private val softAt = Instant.parse("2026-01-01T00:00:00Z")
    private val hardAt = Instant.parse("2026-01-01T00:05:00Z")

    private val nanosPerSecond = 1_000_000_000L

    /** Encode an instant into an arrival stamp's monotonic field, and back, so the stub's
      * `wallClockOf` round-trips the confirmation moments the tests assert on.
      */
    private def stampFor(t: Instant): ArrivalStamp =
        ArrivalStamp(0, t.getEpochSecond * nanosPerSecond + t.getNano)
    private def instantOf(stamp: ArrivalStamp): Instant =
        Instant.ofEpochSecond(
          stamp.monotonicNanos / nanosPerSecond,
          stamp.monotonicNanos % nanosPerSecond
        )

    /** A minimal minor brief for block 1, timed off the head's slot config. */
    private def mkMinorBrief1: IO[BlockBrief.Minor] =
        for {
            now <- realTimeQuantizedInstant(headConfig.slotConfig)
        } yield {
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

    /** A hard-confirmation record with no signatures — the details handler reads only the
      * timestamp, never the effects.
      */
    private val hardRecord: StackEffects.HardConfirmed =
        StackEffects.HardConfirmed.Regular(
          cats.data.NonEmptyList.of(
            PartitionEffects.Minor(
              sec = StandaloneEvacuationCommitment.MultiSigned(
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
                headerMultiSigned = List.empty
              ),
              refunds = List.empty
            )
          )
        )

    /** A stub reader over one block-1 brief plus optional confirmation records. */
    private def stubReader(
        brief: BlockBrief.Minor,
        soft: Boolean,
        hard: Boolean
    ): ConsensusStoreReader[IO] =
        new ConsensusStoreReader[IO]:
            def blockBriefs: IO[List[BlockBrief.Next]] = IO.pure(List(brief))
            def blockBrief(num: BlockNumber): IO[Option[BlockBrief.Next]] =
                IO.pure(Option.when(num == BlockNumber(1))(brief))
            def softConfirmation(
                num: BlockNumber
            ): IO[Option[Timestamped[Block.SoftConfirmed.Next]]] =
                IO.pure(
                  Option.when(soft && num == BlockNumber(1))(
                    Timestamped(
                      stampFor(softAt),
                      Block.SoftConfirmed
                          .Minor(
                            brief,
                            headerMultiSigned = List.empty,
                            finalizationRequested = false
                          )
                    )
                  )
                )
            def stackOf(num: BlockNumber): IO[Option[StackNumber]] =
                IO.pure(Option.when(hard && num == BlockNumber(1))(StackNumber(1)))
            def hardConfirmation(
                num: StackNumber
            ): IO[Option[Timestamped[StackEffects.HardConfirmed]]] =
                IO.pure(
                  Option.when(hard && num == StackNumber(1))(
                    Timestamped(stampFor(hardAt), hardRecord)
                  )
                )
            def requestsOf(peer: HeadPeerNumber): IO[List[Timestamped[UserRequestWithId]]] =
                IO.pure(Nil)
            def request(id: RequestId): IO[Option[Timestamped[UserRequestWithId]]] =
                IO.pure(None)
            def stackBrief(num: StackNumber): IO[Option[StackBrief]] = IO.pure(None)
            def effectStack(l1TxId: TransactionHash): IO[Option[StackNumber]] = IO.pure(None)
            def wallClockOf(stamp: ArrivalStamp): IO[Option[Instant]] =
                IO.pure(Some(instantOf(stamp)))
            def requestBlock(id: RequestId): IO[Option[RequestBlockEntry]] = IO.pure(None)

    private def withRoutes(reader: ConsensusStoreReader[IO])(check: HttpApp[IO] => IO[Unit]): Unit =
        ActorSystem[IO]("HeadBlocksEndpointsTest")
            .use { system =>
                for {
                    requestSequencerStub <- system.actorOf(
                      new Actor[IO, RequestSequencer.Request] {
                          override def receive: Receive[IO, RequestSequencer.Request] =
                              _ => IO.pure(())
                      }
                    )
                    blockWeaverStub <- system.actorOf(
                      new Actor[IO, BlockWeaver.Request] {
                          override def receive: Receive[IO, BlockWeaver.Request] = _ => IO.pure(())
                      }
                    )
                    routes <- HydrozoaRoutes(
                      requestSequencerStub,
                      blockWeaverStub,
                      IO.pure(NodeStatus.Active),
                      reader,
                      None,
                      headConfig,
                      HydrozoaServer.Config(adminUsername = "admin", adminPassword = "admin"),
                      ContraTracer[IO, HydrozoaHttpEvent](_ => IO.unit)
                    )
                    _ <- check(routes.routes.orNotFound)
                } yield ()
            }
            .unsafeRunSync()

    private def get(app: HttpApp[IO], path: String): IO[(Status, Json)] =
        for {
            resp <- app.run(Request[IO](Method.GET, Uri.unsafeFromString(path)))
            body <- resp.as[Json]
        } yield (resp.status, body)

    test("GET /head/blocks lists block 0 plus the spine briefs, leaders round-robin") {
        mkMinorBrief1
            .flatMap(brief =>
                IO(withRoutes(stubReader(brief, soft = false, hard = false)) { app =>
                    get(app, "/head/blocks").map { (status, body) =>
                        val _ = assert(status == Status.Ok)
                        val rows = body.asArray.get
                        val _ = assert(rows.size == 2)
                        val row0 = rows(0).hcursor
                        val _ = assert(row0.get[Int]("number") == Right(0))
                        val _ = assert(row0.get[String]("blockType") == Right("initial"))
                        val _ = assert(row0.downField("leader").focus.forall(_.isNull))
                        val row1 = rows(1).hcursor
                        val _ = assert(row1.get[Int]("number") == Right(1))
                        val _ = assert(row1.get[String]("blockType") == Right("minor"))
                        val _ = assert(
                          row1.get[Int]("leader") == Right(1 % headConfig.nHeadPeers.convert)
                        )
                        ()
                    }
                })
            )
            .unsafeRunSync()
    }

    test("GET /head/blocks/1 walks the confirmation ladder: PROPOSED, SOFT, HARD") {
        mkMinorBrief1
            .flatMap { brief =>
                def statusOf(
                    soft: Boolean,
                    hard: Boolean
                ): (String, Option[String], Option[String]) =
                    var out: (String, Option[String], Option[String]) = ("", None, None)
                    withRoutes(stubReader(brief, soft, hard)) { app =>
                        get(app, "/head/blocks/1").map { (status, body) =>
                            val _ = assert(status == Status.Ok)
                            val c = body.hcursor.downField("confirmation")
                            out = (
                              c.get[String]("status").toOption.get,
                              c.get[String]("softConfirmedAt").toOption,
                              c.get[String]("hardConfirmedAt").toOption
                            )
                        }
                    }
                    out
                IO {
                    val _ = assert(statusOf(soft = false, hard = false) == ("PROPOSED", None, None))
                    val _ = assert(
                      statusOf(soft = true, hard = false) == ("SOFT", Some(softAt.toString), None)
                    )
                    assert(
                      statusOf(soft = true, hard = true) ==
                          ("HARD", Some(softAt.toString), Some(hardAt.toString))
                    )
                }
            }
            .unsafeRunSync()
    }

    test("GET /head/blocks/0 and /head/blocks/0/header serve the config-derived initial block") {
        mkMinorBrief1
            .flatMap(brief =>
                IO(withRoutes(stubReader(brief, soft = false, hard = false)) { app =>
                    for {
                        details <- get(app, "/head/blocks/0")
                        header <- get(app, "/head/blocks/0/header")
                    } yield {
                        val _ = assert(details._1 == Status.Ok)
                        val _ =
                            assert(details._2.hcursor.get[String]("blockType") == Right("initial"))
                        val _ = assert(header._1 == Status.Ok)
                        ()
                    }
                })
            )
            .unsafeRunSync()
    }

    test("GET /head/blocks/1/header returns the brief's header content") {
        mkMinorBrief1
            .flatMap(brief =>
                IO(withRoutes(stubReader(brief, soft = false, hard = false)) { app =>
                    get(app, "/head/blocks/1/header").map { (status, body) =>
                        val _ = assert(status == Status.Ok)
                        val _ = assert(body.hcursor.downField("blockNum").succeeded)
                        ()
                    }
                })
            )
            .unsafeRunSync()
    }

    test("GET /head/blocks/{unknown} is a 404; a malformed number is a 400, not a 500") {
        mkMinorBrief1
            .flatMap(brief =>
                IO(withRoutes(stubReader(brief, soft = false, hard = false)) { app =>
                    for {
                        missing <- get(app, "/head/blocks/99")
                        missingHeader <- get(app, "/head/blocks/99/header")
                        malformed <- app.run(
                          Request[IO](Method.GET, Uri.unsafeFromString("/head/blocks/oops"))
                        )
                    } yield {
                        val _ = assert(missing._1 == Status.NotFound)
                        val _ = assert(missingHeader._1 == Status.NotFound)
                        val _ = assert(malformed.status.code < 500)
                        ()
                    }
                })
            )
            .unsafeRunSync()
    }
