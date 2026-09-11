package hydrozoa.multisig.ledger.remote

import cats.data.EitherT
import cats.effect.IO
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.ledger.l2.{Destination, L2ScreenError, L2Screener}
import hydrozoa.multisig.ledger.remote.RemoteL2ScreenerEvent.{DepositRejected, ScreenerUnavailable, TxRejected}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder}
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.client.Client
import org.http4s.{Method, Request as Http4sRequest, Uri}
import scalus.uplc.builtin.ByteString

/** Screening for a remote L2 ledger, backed by the ledger's dedicated screening endpoint
  * (`docs/spec/l2-isomorphism.md`) — a stateless sidecar of the remote ledger, reached over its own
  * HTTP connection so screening never touches the mutation transport.
  *
  * The error semantics differ from the mutation path on purpose. A screening *rejection* is a
  * verdict: it surfaces as an [[L2ScreenError]] and the request is refused pre-RequestId. A
  * *transport failure* is not: screening is advisory (the remote ledger re-runs the same checks
  * authoritatively at submission), so an unreachable or misbehaving endpoint fails **open** — the
  * request proceeds unscreened, exactly as if no screener were configured — rather than letting a
  * sidecar outage block the request path the way the mutation transport's retry-forever policy
  * would.
  *
  * A transaction ([[screenTx]]) and a deposit ([[screenDeposit]]) are screened the same way — one
  * POST, the same verdict/transport asymmetry above — differing only in the endpoint and the
  * request body: a transaction self-authenticates through its own witnesses, so its body is just
  * the `l2Payload`, with none of the deposit's reference fields.
  */
final class RemoteL2Screener(
    client: Client[IO],
    screenerUri: Uri,
    tracer: ContraTracer[IO, RemoteL2ScreenerEvent],
) extends L2Screener[IO] {
    import RemoteL2Screener.{*, given}

    override def screenTx(l2Payload: ByteString): EitherT[IO, L2ScreenError, Unit] =
        screen(
          screenerUri / "screen" / "tx",
          screenTxBody(l2Payload),
          TxRejected(_),
          "transaction rejected by the remote screener",
        )

    override def screenDeposit(req: L2Screener.ScreenDeposit): EitherT[IO, L2ScreenError, Unit] =
        screen(
          screenerUri / "screen" / "deposit",
          req.asJson,
          DepositRejected(_),
          "deposit rejected by the remote screener",
        )

    /** POST a screening request and map its verdict: `ok: true` passes, `ok: false` is a rejection
      * (traced via `rejected`, its reason surfaced as an [[L2ScreenError]]), and any transport
      * failure fails open — unscreened — as documented on the class.
      */
    private def screen(
        endpoint: Uri,
        body: io.circe.Json,
        rejected: Option[String] => RemoteL2ScreenerEvent,
        defaultReason: String,
    ): EitherT[IO, L2ScreenError, Unit] =
        EitherT(
          client
              .expect[ScreenResponse](Http4sRequest[IO](Method.POST, endpoint).withEntity(body))
              .flatMap {
                  case ScreenResponse(true, _) => IO.pure(Right(()))
                  case ScreenResponse(false, reason) =>
                      tracer
                          .traceWith(rejected(reason))
                          .as(Left(L2ScreenError(reason.getOrElse(defaultReason))))
              }
              .handleErrorWith(cause => tracer.traceWith(ScreenerUnavailable(cause)).as(Right(())))
        )
}

object RemoteL2Screener {

    /** Accepts every request without calling anything: the behaviour of a remote node with no
      * `remoteScreenerUri` configured (and of every remote node before the endpoint existed). The
      * remote ledger still screens authoritatively at submission.
      */
    val passthrough: L2Screener[IO] = new L2Screener[IO] {
        override def screenTx(l2Payload: ByteString): EitherT[IO, L2ScreenError, Unit] =
            EitherT.rightT[IO, L2ScreenError](())

        override def screenDeposit(
            req: L2Screener.ScreenDeposit
        ): EitherT[IO, L2ScreenError, Unit] =
            EitherT.rightT[IO, L2ScreenError](())
    }

    /** The endpoint's verdict: `ok: true` passes, `ok: false` carries the free-form rejection
      * reason. Both verdicts arrive as HTTP 200 — a non-2xx or an undecodable body is a transport
      * failure (handled as "unscreened"), never a rejection.
      */
    final case class ScreenResponse(ok: Boolean, reason: Option[String])

    object ScreenResponse {
        given Decoder[ScreenResponse] = io.circe.generic.semiauto.deriveDecoder
    }

    /** `POST /screen/deposit` body: [[L2Screener.ScreenDeposit]] on the wire — the
      * `RegisterDeposit` fields minus the consensus-assigned ones (which do not exist at screening
      * time), in the same SugarRush-compatible encodings the ws mutation path uses
      * ([[hydrozoa.multisig.ledger.l2.L2LedgerCommand.RegisterDeposit]]'s encoder).
      */
    // AsObject so RegisterDeposit's encoder can graft the consensus fields onto this body in one
    // pass (JsonObject.add) rather than serializing twice and deepMerging.
    given screenDepositEncoder: Encoder.AsObject[L2Screener.ScreenDeposit] = {
        import Destination.given
        import RemoteL2LedgerCodecs.{given_Encoder_Value, given_Encoder_Coin}
        import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.{valueEncoder as _, valueDecoder as _, coinDecoder as _, coinEncoder as _, given}
        Encoder.AsObject.instance { (r: L2Screener.ScreenDeposit) =>
            io.circe.JsonObject(
              "depositId" -> r.depositId.asJson,
              "depositFee" -> r.depositFee.asJson,
              "depositL2Value" -> r.depositL2Value.asJson,
              "refundDestination" -> r.refundDestination.asJson,
              "l2Payload" -> summon[io.circe.Encoder[ByteString]].apply(r.l2Payload)
            )
        }
    }

    /** `POST /screen/tx` body: just the transaction's `l2Payload` (hex), the sole input the
      * stateless tx screen needs — a transaction self-authenticates through its own witnesses, so
      * it carries none of the deposit's reference fields. Mirrors the SugarRush `ScreenTxRequest`.
      */
    def screenTxBody(l2Payload: ByteString): io.circe.Json = {
        import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
        io.circe.Json.obj("l2Payload" -> summon[Encoder[ByteString]].apply(l2Payload))
    }
}
