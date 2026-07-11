package hydrozoa.multisig.server

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{BlockWeaver, RequestSequencer}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger
import hydrozoa.multisig.ledger.eutxol2.store.InMemoryL2Store
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l2.L2LedgerCommand
import io.circe.{Json, Printer}
import org.http4s.circe.*
import org.http4s.implicits.*
import org.http4s.{HttpApp, Method, Request, Status, Uri}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.ShelleyAddress

/** End-to-end demo + test for the L2 query endpoints (`GET /api/l2/utxos/{address}`,
  * `GET /api/l2/transactions`). Boots an in-memory [[EutxoL2Ledger]], seeds it (genesis utxos plus
  * a few applied commands), builds the real [[HydrozoaRoutes]] against it with stub consensus
  * actors, and drives both endpoints through the HTTP layer — asserting the responses and printing
  * the JSON so the flow can be shown in a screen recording.
  */
class L2QueryEndpointsTest extends AnyFunSuite:

    private val printer = Printer.spaces2.copy(dropNullValues = false)

    /** A deterministic single-generator config; nodeConfig is the EUTXO ledger config, headConfig
      * feeds the routes.
      */
    private val multiNodeConfig: MultiNodeConfig =
        MultiNodeConfig.generateDefault
            .pureApply(Gen.Parameters.default, Seed(0L))
    private val nodeConfig = multiNodeConfig.nodeConfigs(HeadPeerNumber.zero)

    /** A refunded-deposit decision at `blockNum` — a real command that logs (so it shows up in
      * `/api/l2/transactions`) without needing a constructed deposit/tx payload: `refundedDeposits`
      * are only removed from the pending set, which tolerates ids that were never registered.
      */
    private def refundDecision(
        blockNum: Int,
        refunded: RequestId
    ): L2LedgerCommand.ApplyDepositDecisions =
        L2LedgerCommand.ApplyDepositDecisions(
          blockNumber = BlockNumber(blockNum),
          blockCreationEndTime = BigInt(blockNum),
          absorbedDeposits = Nil,
          refundedDeposits = List(refunded)
        )

    /** Build the routes against a seeded ledger + stub actors, then run `check` with the HTTP app
      * and the ledger's genesis utxo addresses.
      */
    private def withSeededRoutes(
        check: (HttpApp[IO], EutxoL2Ledger) => IO[Unit]
    ): Unit =
        ActorSystem[IO]("L2QueryEndpointsTest")
            .use { system =>
                for {
                    store <- InMemoryL2Store.create
                    ledger <- EutxoL2Ledger(nodeConfig, store)
                    // Seed a small transaction log: three refunded-deposit decisions, blocks 1..3.
                    _ <- List(1, 2, 3).traverse_ { n =>
                        ledger
                            .sendApplyDepositDecisions(refundDecision(n, RequestId(0, n.toLong)))
                            .value
                            .flatMap(IO.fromEither)
                    }
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
                      Some(ledger),
                      multiNodeConfig.headConfig,
                      HydrozoaServer.Config(adminUsername = "admin", adminPassword = "admin"),
                      ContraTracer[IO, HydrozoaHttpEvent](_ => IO.unit)
                    )
                    _ <- check(routes.routes.orNotFound, ledger)
                } yield ()
            }
            .unsafeRunSync()

    /** Build the routes with **no** L2 reader — the wiring an `any-remote` node gets — and run
      * `check`. No ledger is seeded because a remote-ledger node exposes no L2-query state.
      */
    private def withNoReaderRoutes(check: HttpApp[IO] => IO[Unit]): Unit =
        ActorSystem[IO]("L2QueryEndpointsTest-noReader")
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
                      None,
                      multiNodeConfig.headConfig,
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

    /** The Shelley addresses present in the genesis L2 utxo set (deterministic under `Seed(0L)`).
      */
    private val genesisShelleyAddresses: List[ShelleyAddress] =
        EutxoL2Ledger.State
            .genesis(nodeConfig)
            .activeUtxos
            .values
            .map(_.address)
            .collect { case s: ShelleyAddress => s }
            .toList

    test("GET /api/l2/utxos/{address} returns the address's current L2 utxos, CIP-0116-style") {
        val shelley = genesisShelleyAddresses.headOption
            .getOrElse(fail("genesis produced no Shelley L2 utxos to query"))
        val bech32 = shelley.toBech32.getOrElse(shelley.toHex)
        withSeededRoutes { (app, ledger) =>
            for {
                expected <- ledger.utxosByAddress(shelley)
                result <- get(app, s"/api/l2/utxos/$bech32")
                (status, body) = result
                _ <- IO.println(s"[demo] GET /api/l2/utxos/$bech32")
                _ <- IO.println(printer.print(body))
                _ <- IO(assert(status == Status.Ok))
                _ <- IO(assert(expected.nonEmpty, "the chosen genesis address controls no utxos"))
                _ <- IO(
                  assert(
                    body.asArray.exists(_.size == expected.size),
                    s"expected ${expected.size} utxos, got ${body.asArray.map(_.size)}"
                  )
                )
                // Every entry carries the full CIP-0116 shape: a structured input, and an output
                // with address and value {coin, assets}. `datum` is optional (omitted when absent).
                _ <- IO(
                  assert(
                    body.asArray.forall(_.forall { utxo =>
                        val c = utxo.hcursor
                        c.downField("input").downField("transaction_id").succeeded
                        && c.downField("input").downField("index").succeeded
                        && c.downField("output").downField("address").succeeded
                        && c.downField("output").downField("value").downField("coin").succeeded
                    }),
                    "each utxo must have the CIP-0116 input/output/value shape"
                  )
                )
            } yield ()
        }
    }

    test("GET /api/l2/utxos/{valid address with no utxos} returns an empty array") {
        // A well-formed bech32 address the ledger holds nothing for: the head's own L1 multisig
        // address, which is never an L2 output address.
        val unfunded = multiNodeConfig.headConfig.headMultisigAddress.toBech32
            .getOrElse(fail("head multisig address is not bech32-encodable"))
        withSeededRoutes { (app, _) =>
            for {
                result <- get(app, s"/api/l2/utxos/$unfunded")
                (status, body) = result
                _ <- IO(assert(status == Status.Ok))
                _ <- IO(assert(body.asArray.exists(_.isEmpty), s"expected [], got $body"))
            } yield ()
        }
    }

    test("GET /api/l2/utxos/{malformed} is a 400, not a 500") {
        withSeededRoutes { (app, _) =>
            for {
                result <- get(app, "/api/l2/utxos/not-a-real-address")
                (status, _) = result
                _ <- IO(assert(status == Status.BadRequest))
            } yield ()
        }
    }

    test("GET /api/l2/transactions returns recent applied L2 transactions, newest first") {
        withSeededRoutes { (app, _) =>
            for {
                result <- get(app, "/api/l2/transactions?count=10")
                (status, body) = result
                _ <- IO.println("[demo] GET /api/l2/transactions?count=10")
                _ <- IO.println(printer.print(body))
                _ <- IO(assert(status == Status.Ok))
                entries = body.asArray.getOrElse(Vector.empty)
                _ <- IO(assert(entries.size == 3, s"expected 3 entries, got ${entries.size}"))
                // Newest first: block numbers descend 3, 2, 1; every entry is a deposit refund.
                blockNums = entries.flatMap(_.hcursor.downField("blockNumber").as[Int].toOption)
                _ <- IO(assert(blockNums == Vector(3, 2, 1), s"block order was $blockNums"))
                kinds = entries.flatMap(_.hcursor.downField("kind").as[String].toOption)
                _ <- IO(assert(kinds.forall(_ == "depositRefunded"), s"kinds were $kinds"))
                // requestId is the object shape {headPeerNumber, requestNumber} (the HTTP default).
                _ <- IO(
                  assert(
                    entries.forall { e =>
                        e.hcursor.downField("requestId").downField("headPeerNumber").succeeded
                        && e.hcursor.downField("requestId").downField("requestNumber").succeeded
                    },
                    "requestId must be the object shape"
                  )
                )
            } yield ()
        }
    }

    test("GET /api/l2/transactions?count=1 honors the limit") {
        withSeededRoutes { (app, _) =>
            for {
                result <- get(app, "/api/l2/transactions?count=1")
                (status, body) = result
                _ <- IO(assert(status == Status.Ok))
                _ <- IO(assert(body.asArray.exists(_.size == 1)))
            } yield ()
        }
    }

    test("GET /api/l2/transactions with no ?count returns all seeded entries (default limit)") {
        withSeededRoutes { (app, _) =>
            for {
                result <- get(app, "/api/l2/transactions")
                (status, body) = result
                _ <- IO(assert(status == Status.Ok))
                _ <- IO(assert(body.asArray.exists(_.size == 3)))
            } yield ()
        }
    }

    test("GET /api/l2/transactions?count=0 returns an empty array") {
        withSeededRoutes { (app, _) =>
            for {
                result <- get(app, "/api/l2/transactions?count=0")
                (status, body) = result
                _ <- IO(assert(status == Status.Ok))
                _ <- IO(assert(body.asArray.exists(_.isEmpty)))
            } yield ()
        }
    }

    test("the generated OpenAPI spec is served under /docs") {
        withSeededRoutes { (app, _) =>
            for {
                resp <- app.run(Request[IO](Method.GET, Uri.unsafeFromString("/docs/docs.yaml")))
                body <- resp.bodyText.compile.string
                _ <- IO(
                  assert(resp.status == Status.Ok, s"expected 200 for the spec, got ${resp.status}")
                )
                _ <- IO(
                  assert(body.contains("openapi"), s"spec body unexpected: ${body.take(60)}")
                )
            } yield ()
        }
    }

    test("GET /api/l2/transactions?count=abc is a 400, not a 404") {
        withSeededRoutes { (app, _) =>
            // tapir rejects an un-decodable query param with a 400 and a plain-text body, so check
            // the status directly rather than through the JSON helper.
            for {
                resp <- app.run(
                  Request[IO](Method.GET, Uri.unsafeFromString("/api/l2/transactions?count=abc"))
                )
                _ <- IO(
                  assert(resp.status == Status.BadRequest, s"expected 400, got ${resp.status}")
                )
            } yield ()
        }
    }

    test("with no L2 reader (an any-remote node), the L2 query routes are absent (404)") {
        // A well-formed bech32 address the L2 endpoints would happily answer (200) if mounted; with
        // reader=None the routes are not mounted at all, so it must be a 404. A core route (admin)
        // still answers with a 401 challenge, proving only the L2 endpoints were dropped.
        val validAddr = multiNodeConfig.headConfig.headMultisigAddress.toBech32
            .getOrElse(fail("head multisig address is not bech32-encodable"))
        val badAuth =
            org.http4s.headers.Authorization(org.http4s.BasicCredentials("admin", "wrong"))
        withNoReaderRoutes { app =>
            for {
                utxos <- app.run(
                  Request[IO](Method.GET, Uri.unsafeFromString(s"/api/l2/utxos/$validAddr"))
                )
                _ <- IO(
                  assert(
                    utxos.status == Status.NotFound,
                    s"expected 404 for /api/l2/utxos with no reader, got ${utxos.status}"
                  )
                )
                txs <- app.run(
                  Request[IO](Method.GET, Uri.unsafeFromString("/api/l2/transactions"))
                )
                _ <- IO(
                  assert(
                    txs.status == Status.NotFound,
                    s"expected 404 for /api/l2/transactions with no reader, got ${txs.status}"
                  )
                )
                admin <- app.run(
                  Request[IO](Method.POST, Uri.unsafeFromString("/api/admin/finalize"))
                      .putHeaders(badAuth)
                )
                _ <- IO(
                  assert(
                    admin.status == Status.Unauthorized,
                    s"a core route should still be mounted (401), got ${admin.status}"
                  )
                )
            } yield ()
        }
    }

    test(
      "POST /api/admin/finalize with wrong credentials is 401 with a WWW-Authenticate challenge"
    ) {
        withSeededRoutes { (app, _) =>
            // The harness configures admin/admin; send a wrong password.
            val badAuth =
                org.http4s.headers.Authorization(org.http4s.BasicCredentials("admin", "wrong"))
            for {
                resp <- app.run(
                  Request[IO](Method.POST, Uri.unsafeFromString("/api/admin/finalize"))
                      .putHeaders(badAuth)
                )
                _ <- IO(
                  assert(resp.status == Status.Unauthorized, s"expected 401, got ${resp.status}")
                )
                hasChallenge = resp.headers.headers.exists(
                  _.name.toString.equalsIgnoreCase("WWW-Authenticate")
                )
                _ <- IO(
                  assert(hasChallenge, "a 401 from the admin endpoint must carry WWW-Authenticate")
                )
            } yield ()
        }
    }

end L2QueryEndpointsTest
