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
import hydrozoa.multisig.metrics.PeerMetrics
import hydrozoa.multisig.persistence.{ArrivalStamp, ConsensusStoreReader, DepositDecision, RequestBlockEntry, Timestamped}
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
  * the request-details lifecycle ladder (UNPROCESSED → PROPOSED → SOFT_CONFIRMED → HARD_CONFIRMED),
  * plus the 404 / 400 edges.
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
        hardStack: Option[(BlockNumber, StackNumber, Instant)] = None,
        decisionRow: Option[DepositDecision] = None
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
            def decision(id: RequestId): IO[Option[DepositDecision]] = IO.pure(decisionRow)
            def withdrawalEffects(id: RequestId): IO[List[TransactionHash]] = IO.pure(Nil)
            def stackBrief(num: StackNumber): IO[Option[StackBrief]] = IO.pure(None)
            def effectStack(l1TxId: TransactionHash): IO[Option[StackNumber]] = IO.pure(None)
            def wallClockOf(stamp: ArrivalStamp): IO[Instant] =
                IO.pure(instantOf(stamp))

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
                      Some(requestSequencerStub),
                      blockWeaverStub,
                      IO.pure(NodeStatus.Active),
                      reader,
                      None,
                      headConfig,
                      HydrozoaServer.Config(adminUsername = "admin", adminPassword = "admin"),
                      PeerMetrics.create(0L, Vector.empty),
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

    test("GET /head/requests is not mounted — an unpaged full-CF scan is an OOM, not an API") {
        // Keyed lookups stay mounted, so it is the second assertion that carries the test: a
        // router that mounted nothing at all would pass the first one on its own.
        withRoutes(stubReader(Map(peer0 -> List(txRequest(peer0, 0))))) { app =>
            for {
                listing <- app.run(
                  Request[IO](Method.GET, Uri.unsafeFromString("/head/requests"))
                )
                filtered <- app.run(
                  Request[IO](Method.GET, Uri.unsafeFromString("/head/requests?type=deposit"))
                )
                byId <- app.run(Request[IO](Method.GET, Uri.unsafeFromString("/head/requests/0")))
            } yield {
                // 405 rather than 404 on a HEAD node: POST /head/requests is mounted, so the
                // path exists and only the GET verb is gone.
                val _ = assert(
                  listing.status == Status.MethodNotAllowed,
                  s"GET /head/requests must not be mounted, got ${listing.status}"
                )
                val _ = assert(
                  filtered.status == Status.MethodNotAllowed,
                  s"a filtered listing must not be mounted either, got ${filtered.status}"
                )
                val _ = assert(
                  byId.status == Status.Ok,
                  s"GET /head/requests/{id} must still serve, got ${byId.status}"
                )
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
                      s.get[String]("type").toOption.get,
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
              ("PROPOSED", Some(3), None)
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

    test("GET /head/requests/{id} exposes a valid deposit's decision status") {
        val depositId = RequestId(peer0, RequestNumber(0))
        val base = Map(peer0 -> List(depositRequest(peer0, 0)))
        val valid = RequestBlockEntry(BlockNumber(3), ValidityFlag.Valid)

        // (absorptionDecisionStatus.type, blockNumber, decision), or None when the field is absent.
        def decisionOf(
            reader: ConsensusStoreReader[IO]
        ): Option[(String, Option[Int], Option[String])] =
            var out: Option[(String, Option[Int], Option[String])] = None
            withRoutes(reader) { app =>
                get(app, s"/head/requests/${depositId.asI64}").map { (status, body) =>
                    val _ = assert(status == Status.Ok)
                    val d = body.hcursor.downField("absorptionDecisionStatus")
                    out = d
                        .get[String]("type")
                        .toOption
                        .map(s =>
                            (
                              s,
                              d.get[Int]("blockNumber").toOption,
                              d.get[String]("decision").toOption
                            )
                        )
                }
            }
            out

        // A transaction carries no decision status.
        val _ = assert(
          decisionOf(stubReader(Map(peer0 -> List(txRequest(peer0, 0))), processed = Some(valid)))
              == None
        )
        // A valid deposit with no decision row yet is UNPROCESSED.
        val _ = assert(
          decisionOf(stubReader(base, processed = Some(valid))) == Some(("UNPROCESSED", None, None))
        )
        // A decided-but-unconfirmed deposit reports PROPOSED with its block and outcome.
        val _ = assert(
          decisionOf(
            stubReader(
              base,
              processed = Some(valid),
              decisionRow = Some(DepositDecision.Absorbed(BlockNumber(3)))
            )
          ) == Some(("PROPOSED", Some(3), Some("ABSORBED")))
        )
        assert(
          decisionOf(
            stubReader(
              base,
              processed = Some(valid),
              decisionRow = Some(DepositDecision.Rejected(BlockNumber(3)))
            )
          ) == Some(("PROPOSED", Some(3), Some("REJECTED")))
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

    /** Build the routes the way a **coil** peer gets them — `requestSequencer = None`, because a
      * follower accepts no user requests — and run `check`.
      */
    private def withCoilRoutes(check: HttpApp[IO] => IO[Unit]): Unit =
        ActorSystem[IO]("HeadRequestsEndpointsTest-coil")
            .use { system =>
                for {
                    blockWeaverStub <- system.actorOf(
                      new Actor[IO, BlockWeaver.Request] {
                          override def receive: Receive[IO, BlockWeaver.Request] = _ => IO.pure(())
                      }
                    )
                    routes <- HydrozoaRoutes(
                      None,
                      blockWeaverStub,
                      IO.pure(NodeStatus.Active),
                      stubReader(Map.empty),
                      None,
                      headConfig,
                      HydrozoaServer.Config(adminUsername = "admin", adminPassword = "admin"),
                      PeerMetrics.create(0L, Vector.empty),
                      ContraTracer[IO, HydrozoaHttpEvent](_ => IO.unit)
                    )
                    _ <- check(routes.routes.orNotFound)
                } yield ()
            }
            .unsafeRunSync()

    test(
      "on a coil peer (no request sequencer) the mutating routes are absent, reads still serve"
    ) {
        // The coil exists to be observable: its read surface must answer, or the whole point of
        // giving it a server is lost. So this asserts both halves — the mutations are gone AND a
        // read still works. Without the second assertion a totally unmounted router would pass.
        withCoilRoutes { app =>
            for {
                submit <- app.run(
                  Request[IO](Method.POST, Uri.unsafeFromString("/head/requests"))
                )
                finalize <- app.run(
                  Request[IO](Method.POST, Uri.unsafeFromString("/api/admin/finalize"))
                )
                stats <- app.run(Request[IO](Method.GET, Uri.unsafeFromString("/head/stats")))
                ready <- app.run(Request[IO](Method.GET, Uri.unsafeFromString("/ready")))
            } yield {
                // 404, not 405: a coil mounts neither verb, so the path does not exist there.
                val _ = assert(
                  submit.status == Status.NotFound,
                  s"POST /head/requests must not be mounted on a coil, got ${submit.status}"
                )
                // 404, not the 401 challenge a mounted admin route would give.
                val _ = assert(
                  finalize.status == Status.NotFound,
                  s"POST /api/admin/finalize must not be mounted on a coil, got ${finalize.status}"
                )
                val _ = assert(
                  stats.status == Status.Ok,
                  s"GET /head/stats must serve on a coil, got ${stats.status}"
                )
                val _ = assert(
                  ready.status == Status.Ok,
                  s"GET /ready must serve on a coil, got ${ready.status}"
                )
                ()
            }
        }
    }
