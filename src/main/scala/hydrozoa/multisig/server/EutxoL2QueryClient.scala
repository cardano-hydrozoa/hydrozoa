package hydrozoa.multisig.server

import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.multisig.ledger.l2.EutxoL2LedgerReader
import hydrozoa.multisig.server.ApiDto.{L2TxSummaryView, L2UtxoView, given}
import org.http4s.Uri
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.client.Client
import scala.util.Try
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AssetName, Coin, ScriptHash, TransactionHash, TransactionInput, TransactionOutput, Value}

/** A client-side handle for the read-only L2 queries a Hydrozoa peer exposes — the counterpart to
  * [[SubmissionClient]]'s write path. Abstracts over the transport: a direct read of a local
  * ledger, an in-memory http4s round-trip against [[HydrozoaRoutes]], or a real over-the-wire HTTP
  * call.
  *
  * Named for the ledger kind rather than the endpoints: these queries exist only for the EUTXO
  * reference ledger (`/l2/cardano-eutxo/…`), and a node wired to a remote ledger mounts none of
  * them — see [[EutxoL2LedgerReader]].
  */
trait EutxoL2QueryClient:
    /** The L2 utxos `address` controls right now, as scalus values ready to be spent. */
    def utxos(address: ShelleyAddress): IO[List[(TransactionInput, TransactionOutput)]]

    /** The most recently applied L2 transactions, newest first, at most `count`. */
    def transactions(count: Int): IO[List[L2TxSummaryView]]

object EutxoL2QueryClient:

    /** In-process impl reading a peer's own ledger through the same narrow view the HTTP server
      * gets. Mirrors [[SubmissionClient.direct]].
      */
    def direct(reader: EutxoL2LedgerReader[IO]): EutxoL2QueryClient =
        new EutxoL2QueryClient:
            def utxos(address: ShelleyAddress): IO[List[(TransactionInput, TransactionOutput)]] =
                reader.utxosByAddress(address).map(_.toList)

            def transactions(count: Int): IO[List[L2TxSummaryView]] =
                reader.recentTransactions(count).map(_.map(ApiDto.mkL2TxSummaryView).toList)

    /** http4s-based impl over the two documented query endpoints. `client` can be a real http4s
      * `Client[IO]` or an in-memory `Client.fromHttpApp` — the same split [[SubmissionClient.http]]
      * supports.
      *
      * The utxo response is decoded back into scalus types here rather than by each caller: every
      * consumer wants to spend the outputs, not render the wire shape.
      */
    def http(client: Client[IO], baseUri: Uri): EutxoL2QueryClient =
        new EutxoL2QueryClient:
            def utxos(address: ShelleyAddress): IO[List[(TransactionInput, TransactionOutput)]] =
                for {
                    bech32 <- IO.fromOption(address.toBech32.toOption)(
                      RuntimeException(s"address is not bech32-renderable: $address")
                    )
                    views <- client.expect[List[L2UtxoView]](
                      baseUri / "l2" / "cardano-eutxo" / "utxos" / bech32
                    )
                    parsed <- IO.fromEither(
                      views
                          .traverse(parseUtxoView)
                          .left
                          .map(e => RuntimeException(s"could not parse the L2 utxos response: $e"))
                    )
                } yield parsed

            def transactions(count: Int): IO[List[L2TxSummaryView]] =
                client.expect[List[L2TxSummaryView]](
                  (baseUri / "l2" / "cardano-eutxo" / "transactions")
                      .withQueryParam("count", count)
                )

    /** Parse one `GET /l2/cardano-eutxo/utxos/{address}` entry back into scalus types.
      * Datum-bearing utxos are accepted for display but their datum is not reconstructed — the
      * input reference is what the tx spends.
      */
    private def parseUtxoView(
        view: L2UtxoView
    ): Either[String, (TransactionInput, TransactionOutput)] =
        for {
            txId <- Try(TransactionHash.fromHex(view.input.transaction_id)).toEither.left
                .map(e => s"bad transaction_id: ${e.getMessage}")
            address <- Try(Address.fromBech32(view.output.address)).toEither.left
                .map(e => s"bad address: ${e.getMessage}")
            shelley <- address match {
                case sa: ShelleyAddress => Right(sa)
                case other              => Left(s"not a Shelley address: $other")
            }
            coin <- view.output.value.coin.toLongOption.toRight(
              s"bad coin: ${view.output.value.coin}"
            )
            assets <- parseAssets(view)
        } yield TransactionInput(txId, view.input.index) ->
            Babbage(shelley, Value.assets(assets, Coin(coin)), None, None)

    /** The view's native assets, keyed by policy id then asset name, with every hex string and
      * quantity validated.
      */
    private def parseAssets(
        view: L2UtxoView
    ): Either[String, Map[ScriptHash, Map[AssetName, Long]]] =
        view.output.value.assets.toList
            .traverse { case (policyHex, byAsset) =>
                for {
                    policy <- Try(ScriptHash.fromHex(policyHex)).toEither.left
                        .map(e => s"bad policy id: ${e.getMessage}")
                    parsed <- byAsset.toList.traverse { case (nameHex, qty) =>
                        qty.toLongOption
                            .toRight(s"bad asset quantity: $qty")
                            .map(q => AssetName.fromHex(nameHex) -> q)
                    }
                } yield policy -> parsed.toMap
            }
            .map(_.toMap)
