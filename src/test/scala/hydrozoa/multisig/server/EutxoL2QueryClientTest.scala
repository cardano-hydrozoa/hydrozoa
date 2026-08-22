package hydrozoa.multisig.server

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.multisig.server.ApiDto.{L2TxKindView, L2TxSummaryView, RequestIdView, mkL2UtxoView, given}
import io.circe.Json
import io.circe.syntax.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.client.Client
import org.http4s.{HttpApp, Response, Status, Uri}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AssetName, Coin, ScriptHash, TransactionHash, TransactionInput, TransactionOutput, Value}

/** Round-trip coverage for [[EutxoL2QueryClient.http]] against the server's own view builders.
  *
  * The client's decode has to be the exact inverse of [[ApiDto.mkL2UtxoView]]: the server encodes
  * an L2 utxo, the client turns it back into the scalus values a caller will spend. Serving
  * `mkL2UtxoView`'s own output through an in-memory `Client.fromHttpApp` — the transport split
  * [[SubmissionClient]] already relies on — pins the two halves together, so changing one without
  * the other fails here rather than in the heavy Docker suite.
  */
class EutxoL2QueryClientTest extends AnyFunSuite:

    private val baseUri = Uri.unsafeFromString("http://head-0:8080")

    private val address: ShelleyAddress = Address
        .fromBech32("addr_test1vrhh0xnmqlh5jpys4cqrj3vteje70r0swakm7q2w8nmcp3sh5wdk4")
        .asInstanceOf[ShelleyAddress]

    private val input = TransactionInput(
      TransactionHash.fromHex(
        "9844228688a4d0e54ec416bf7aa31fc10888d5845bfb16cbd68fb625ff86bb5f"
      ),
      3
    )

    private val policy: ScriptHash =
        ScriptHash.fromHex("a217f9484e3b7854ff68242bd37600da6b734c1b467a6d4e902aac07")

    /** Ada plus a native asset, so the round trip covers the whole value shape, not just coin. */
    private val output: TransactionOutput = Babbage(
      address,
      Value.assets(Map(policy -> Map(AssetName.fromHex("745348454e") -> 7L)), Coin(4_500_000L)),
      None,
      None
    )

    /** Answer every request with `body`, ignoring the path — the request itself is asserted
      * separately where it matters.
      */
    private def serving(body: Json): Client[IO] =
        Client.fromHttpApp(HttpApp[IO](_ => IO.pure(Response[IO](Status.Ok).withEntity(body))))

    test("a served L2 utxo decodes back into the input and output it was built from") {
        val served = List(mkL2UtxoView(input, output)).asJson
        val decoded =
            EutxoL2QueryClient.http(serving(served), baseUri).utxos(address).unsafeRunSync()
        assert(decoded == List(input -> output))
    }

    test("an empty utxo set decodes to an empty list") {
        val decoded = EutxoL2QueryClient
            .http(serving(List.empty[ApiDto.L2UtxoView].asJson), baseUri)
            .utxos(address)
            .unsafeRunSync()
        assert(decoded.isEmpty)
    }

    test("a malformed utxo entry fails rather than being silently dropped") {
        // A well-typed response the *parser* must reject: transaction_id is a String on the wire,
        // so this decodes as a DTO and only fails once turned back into a TransactionHash.
        val corrupted = Json.arr(
          mkL2UtxoView(input, output).asJson.deepMerge(
            Json.obj(
              "input" -> Json.obj(
                "transaction_id" -> Json.fromString("not-hex"),
                "index" -> Json.fromInt(0)
              )
            )
          )
        )
        val result = EutxoL2QueryClient
            .http(serving(corrupted), baseUri)
            .utxos(address)
            .attempt
            .unsafeRunSync()
        assert(
          result.left.exists(_.getMessage.contains("bad transaction_id")),
          s"expected the parser to reject the entry, got: $result"
        )
    }

    test("the recent-transactions feed decodes and forwards its count") {
        val summary =
            L2TxSummaryView(RequestIdView(1, 42L), blockNumber = 7, L2TxKindView.Transaction)
        var requested: Option[Uri] = None
        val client = Client.fromHttpApp(HttpApp[IO] { request =>
            requested = Some(request.uri)
            IO.pure(Response[IO](Status.Ok).withEntity(List(summary).asJson))
        })
        val decoded = EutxoL2QueryClient.http(client, baseUri).transactions(25).unsafeRunSync()
        assert(
          decoded == List(summary) &&
              requested.exists(_.query.params.get("count").contains("25")),
          s"count must reach the server; requested=$requested"
        )
    }
