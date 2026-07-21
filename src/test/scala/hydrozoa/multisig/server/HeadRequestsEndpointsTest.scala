package hydrozoa.multisig.server

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.NodeStatus
import hydrozoa.multisig.consensus.UserRequest.{DepositRequest, TransactionRequest}
import hydrozoa.multisig.consensus.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{BlockWeaver, RequestSequencer, UserRequestWithId}
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.stack.{StackBrief, StackEffects, StackNumber}
import hydrozoa.multisig.persistence.{ArrivalStamp, ConsensusStoreReader, RequestBlockEntry, Timestamped}
import io.circe.Json
import java.time.Instant
import org.http4s.circe.*
import org.http4s.implicits.*
import org.http4s.{HttpApp, Method, Request, Status, Uri}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.TransactionHash
import scalus.uplc.builtin.ByteString

/** The `/head/requests` queries through the HTTP layer against a stubbed [[ConsensusStoreReader]]:
  * the listing (with its `?type=` / `?peer_number=` filters), keyed by the opaque request id, and
  * the request-details lifecycle ladder (UNPROCESSED → LOCALLY_PROCESSED → SOFT_CONFIRMED →
  * HARD_CONFIRMED), plus the 404 / 400 edges.
  */
class HeadRequestsEndpointsTest extends AnyFunSuite:

    private val multiNodeConfig: MultiNodeConfig =
        MultiNodeConfig.generateDefault.pureApply(Gen.Parameters.default, Seed(0L))
    private val headConfig = multiNodeConfig.headConfig
    private val peer0 = HeadPeerNumber(0)

    private val receivedAt = Instant.parse("2026-01-01T00:00:00Z")
    private val softAt = Instant.parse("2026-01-01T00:01:00Z")
    private val hardAt = Instant.parse("2026-01-01T00:05:00Z")

    private val nanosPerSecond = 1_000_000_000L

    /** Encode an instant into an arrival stamp's monotonic field, and back, so the stub's
      * `wallClockOf` round-trips the moments the tests assert on.
      */
    private def stampFor(t: Instant): ArrivalStamp =
        ArrivalStamp(0, t.getEpochSecond * nanosPerSecond + t.getNano)
    private def instantOf(stamp: ArrivalStamp): Instant =
        Instant.ofEpochSecond(
          stamp.monotonicNanos / nanosPerSecond,
          stamp.monotonicNanos % nanosPerSecond
        )

    private def txRequest(peer: HeadPeerNumber, num: Long): UserRequestWithId =
        UserRequestWithId(
          TransactionRequest(TransactionRequestBody(ByteString.empty)),
          RequestId(peer, RequestNumber(num))
        )

    private def depositRequest(peer: HeadPeerNumber, num: Long): UserRequestWithId =
        UserRequestWithId(
          DepositRequest(DepositRequestBody(ByteString.empty, ByteString.empty)),
          RequestId(peer, RequestNumber(num))
        )

    /** A reader over a fixed per-peer request map (each request stamped at `receivedAt`), plus an
      * optional lifecycle for request `(peer0, 0)`: its processing block/verdict and that block's
      * confirmation moments.
      */
    private def stubReader(
        requests: Map[HeadPeerNumber, List[UserRequestWithId]],
        processed: Option[RequestBlockEntry] = None,
        softBlock: Option[(BlockNumber, Instant)] = None,
        hardStack: Option[(BlockNumber, StackNumber, Instant)] = None
    ): ConsensusStoreReader[IO] =
        new ConsensusStoreReader[IO]:
            def blockBriefs: IO[List[BlockBrief.Next]] = IO.pure(Nil)
            def blockBrief(num: BlockNumber): IO[Option[BlockBrief.Next]] = IO.pure(None)
            def softConfirmation(
                num: BlockNumber
            ): IO[Option[Timestamped[Block.SoftConfirmed.Next]]] =
                IO.pure(softBlock.collect {
                    case (b, at) if b == num => Timestamped(stampFor(at), null)
                })
            def stackOf(num: BlockNumber): IO[Option[StackNumber]] =
                IO.pure(hardStack.collect { case (b, s, _) if b == num => s })
            def hardConfirmation(
                num: StackNumber
            ): IO[Option[Timestamped[StackEffects.HardConfirmed]]] =
                IO.pure(hardStack.collect {
                    case (_, s, at) if s == num => Timestamped(stampFor(at), null)
                })
            def requestsOf(peer: HeadPeerNumber): IO[List[Timestamped[UserRequestWithId]]] =
                IO.pure(
                  requests.getOrElse(peer, Nil).map(r => Timestamped(stampFor(receivedAt), r))
                )
            def request(id: RequestId): IO[Option[Timestamped[UserRequestWithId]]] =
                IO.pure(
                  requests
                      .getOrElse(id.peerNum, Nil)
                      .find(_.requestId == id)
                      .map(r => Timestamped(stampFor(receivedAt), r))
                )
            def requestBlock(id: RequestId): IO[Option[RequestBlockEntry]] =
                IO.pure(
                  processed.filter(_ => id == RequestId(peer0, RequestNumber(0)))
                )
            def absorptionBlock(id: RequestId): IO[Option[BlockNumber]] = IO.pure(None)
            def withdrawalEffects(id: RequestId): IO[List[TransactionHash]] = IO.pure(Nil)
            def stackBrief(num: StackNumber): IO[Option[StackBrief]] = IO.pure(None)
            def effectStack(l1TxId: TransactionHash): IO[Option[StackNumber]] = IO.pure(None)
            def wallClockOf(stamp: ArrivalStamp): IO[Option[Instant]] =
                IO.pure(Some(instantOf(stamp)))

    private def withRoutes(reader: ConsensusStoreReader[IO])(check: HttpApp[IO] => IO[Unit]): Unit =
        ActorSystem[IO]("HeadRequestsEndpointsTest")
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

    test("GET /head/requests lists rows with the opaque i64 id, peer number, and type") {
        val reader = stubReader(
          Map(peer0 -> List(txRequest(peer0, 0), depositRequest(peer0, 1)))
        )
        withRoutes(reader) { app =>
            get(app, "/head/requests").map { (status, body) =>
                val _ = assert(status == Status.Ok)
                val rows = body.asArray.get
                val _ = assert(rows.size == 2)
                val r0 = rows(0).hcursor
                // peer 0, request 0 packs to i64 0.
                val _ = assert(r0.get[Long]("requestId") == Right(0L))
                val _ = assert(r0.get[Int]("peerNumber") == Right(0))
                val _ = assert(r0.get[String]("requestType") == Right("transaction"))
                val r1 = rows(1).hcursor
                val _ = assert(r1.get[Long]("requestId") == Right(1L))
                val _ = assert(r1.get[String]("requestType") == Right("deposit"))
                ()
            }
        }
    }

    test("GET /head/requests?type= and ?peer_number= narrow the listing") {
        val reader = stubReader(
          Map(peer0 -> List(txRequest(peer0, 0), depositRequest(peer0, 1)))
        )
        withRoutes(reader) { app =>
            for {
                deposits <- get(app, "/head/requests?type=deposit")
                thisPeer <- get(app, "/head/requests?peer_number=0")
                otherPeer <- get(app, "/head/requests?peer_number=99")
                unknownType <- get(app, "/head/requests?type=nonsense")
            } yield {
                val _ = assert(deposits._1 == Status.Ok)
                val depRows = deposits._2.asArray.get
                val _ = assert(depRows.size == 1)
                val _ = assert(depRows(0).hcursor.get[String]("requestType") == Right("deposit"))
                val _ = assert(thisPeer._2.asArray.get.size == 2)
                // No such author peer -> empty, not an error.
                val _ = assert(otherPeer._1 == Status.Ok && otherPeer._2.asArray.get.isEmpty)
                val _ = assert(unknownType._2.asArray.get.isEmpty)
                ()
            }
        }
    }

    test("GET /head/requests/{id} walks the lifecycle: UNPROCESSED to HARD_CONFIRMED") {
        def statusOf(reader: ConsensusStoreReader[IO]): (String, Option[Int], Option[String]) =
            var out: (String, Option[Int], Option[String]) = ("", None, None)
            withRoutes(reader) { app =>
                // peer 0, request 0 -> opaque id 0.
                get(app, "/head/requests/0").map { (status, body) =>
                    val _ = assert(status == Status.Ok)
                    val s = body.hcursor.downField("status")
                    out = (
                      s.get[String]("status").toOption.get,
                      s.get[Int]("blockNumber").toOption,
                      s.get[String]("hardConfirmedAt").toOption
                    )
                }
            }
            out

        val base = Map(peer0 -> List(txRequest(peer0, 0)))
        val entry = RequestBlockEntry(BlockNumber(3), ValidityFlag.Valid)

        val _ = assert(statusOf(stubReader(base)) == ("UNPROCESSED", None, None))
        val _ = assert(
          statusOf(stubReader(base, processed = Some(entry))) ==
              ("LOCALLY_PROCESSED", Some(3), None)
        )
        val _ = assert(
          statusOf(
            stubReader(base, processed = Some(entry), softBlock = Some((BlockNumber(3), softAt)))
          ) == ("SOFT_CONFIRMED", Some(3), None)
        )
        assert(
          statusOf(
            stubReader(
              base,
              processed = Some(entry),
              softBlock = Some((BlockNumber(3), softAt)),
              hardStack = Some((BlockNumber(3), StackNumber(1), hardAt))
            )
          ) == ("HARD_CONFIRMED", Some(3), Some(hardAt.toString))
        )
    }

    test("GET /head/requests/{id} carries the opaque id, peer, receive time, and verdict") {
        val reader = stubReader(
          Map(peer0 -> List(txRequest(peer0, 0))),
          processed = Some(RequestBlockEntry(BlockNumber(2), ValidityFlag.Invalid))
        )
        withRoutes(reader) { app =>
            get(app, "/head/requests/0").map { (status, body) =>
                val _ = assert(status == Status.Ok)
                val _ = assert(body.hcursor.get[Long]("requestId") == Right(0L))
                val _ = assert(body.hcursor.get[Int]("peerNumber") == Right(0))
                val _ = assert(body.hcursor.get[String]("receivedAt") == Right(receivedAt.toString))
                val _ = assert(
                  body.hcursor.downField("status").get[String]("validity") == Right("invalid")
                )
                ()
            }
        }
    }

    test("GET /head/requests/{unknown} is a 404; a malformed id is a 4xx, not a 500") {
        val reader = stubReader(Map(peer0 -> List(txRequest(peer0, 0))))
        withRoutes(reader) { app =>
            for {
                missing <- get(app, "/head/requests/99")
                malformed <- app.run(
                  Request[IO](Method.GET, Uri.unsafeFromString("/head/requests/oops"))
                )
                // 2^48 is out of the packable range -> a clean decode error, never a 500.
                outOfRange <- app.run(
                  Request[IO](Method.GET, Uri.unsafeFromString("/head/requests/281474976710656"))
                )
            } yield {
                val _ = assert(missing._1 == Status.NotFound)
                val _ = assert(malformed.status.code >= 400 && malformed.status.code < 500)
                val _ = assert(outOfRange.status.code >= 400 && outOfRange.status.code < 500)
                ()
            }
        }
    }
