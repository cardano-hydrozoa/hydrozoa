package hydrozoa.multisig.ledger.remote

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.ledger.l2.{Destination, L2ScreenError, L2Screener}
import io.circe.syntax.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.client.Client
import org.http4s.dsl.io.*
import org.http4s.{HttpApp, Uri}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.ledger.{Blake2b_256, Coin, Hash, HashPurpose, TransactionInput, Value}
import scalus.uplc.builtin.ByteString

/** The screening client's two contracts: the wire shape of `POST /screen/deposit` (pinned against
  * the golden JSON the SugarRush screener pins on its side,
  * `screener/src/screen.rs::screen_deposit_request_golden`), and the verdict semantics — `ok:
  * false` is a rejection, everything that is not a decodable 200 fails open as "unscreened".
  */
class RemoteL2ScreenerTest extends AnyFunSuite:

    private val refundAddress =
        "addr1z9ryamhgnuz6lau86sqytte2gz5rlktv2yce05e0h3207qkwuyc0kwgcsnu6hcw94vt9nqevfw8axfnujtn6xsg6eq0q5u2up5"

    private val screenDeposit = L2Screener.ScreenDeposit(
      depositId = TransactionInput(
        transactionId = Hash[Blake2b_256, HashPurpose.TransactionHash](
          ByteString.fromHex("a1" * 32)
        ),
        index = 0
      ),
      depositFee = Coin(2_000_000L),
      depositL2Value = Value(Coin(10_000_000L)),
      refundDestination = Destination(
        address = Address.fromBech32(refundAddress),
        datum = None
      ),
      l2Payload = ByteString.fromHex("cafebabe")
    )

    test("the deposit-screen body matches the golden JSON the SugarRush screener pins") {
        import RemoteL2Screener.given
        val expected = io.circe.parser
            .parse(
              s"""{
                 |  "depositId": {"transaction_id": "${"a1" * 32}", "index": 0},
                 |  "depositFee": 2000000,
                 |  "depositL2Value": {"assets": [{"asset": {"tag": "Ada"}, "value": 10000000}]},
                 |  "refundDestination": {"address": "$refundAddress", "datum": null},
                 |  "l2Payload": "cafebabe"
                 |}""".stripMargin
            )
            .toOption
            .get
        assert(screenDeposit.asJson == expected)
    }

    private def screenerAgainst(app: HttpApp[IO]): RemoteL2Screener =
        RemoteL2Screener(
          Client.fromHttpApp(app),
          Uri.unsafeFromString("http://screener.test"),
          ContraTracer.nullTracer,
        )

    private def verdictOf(app: HttpApp[IO]): Either[L2ScreenError, Unit] =
        screenerAgainst(app).screenDeposit(screenDeposit).value.unsafeRunSync()

    test("an ok verdict passes") {
        val app = HttpApp[IO](_ => Ok(io.circe.Json.obj("ok" -> true.asJson)))
        assert(verdictOf(app) == Right(()))
    }

    test("a rejection surfaces its reason as the L2ScreenError message") {
        val app = HttpApp[IO](_ =>
            Ok(io.circe.Json.obj("ok" -> false.asJson, "reason" -> "insufficient ada".asJson))
        )
        assert(verdictOf(app) == Left(L2ScreenError("insufficient ada")))
    }

    test("a reasonless rejection still rejects") {
        val app = HttpApp[IO](_ => Ok(io.circe.Json.obj("ok" -> false.asJson)))
        assert(verdictOf(app).isLeft)
    }

    test("a non-2xx fails open: unscreened, not rejected") {
        val app = HttpApp[IO](_ => InternalServerError("boom"))
        assert(verdictOf(app) == Right(()))
    }

    test("a transport failure fails open: unscreened, not rejected") {
        val app = HttpApp[IO](_ => IO.raiseError(new RuntimeException("connection refused")))
        assert(verdictOf(app) == Right(()))
    }

    test("transactions are not screened remotely: no call is made") {
        var called = false
        val app = HttpApp[IO] { _ => IO { called = true } *> Ok(io.circe.Json.obj()) }
        val verdict =
            screenerAgainst(app).screenTx(ByteString.fromHex("cafebabe")).value.unsafeRunSync()
        assert(verdict == Right(()) && !called)
    }
