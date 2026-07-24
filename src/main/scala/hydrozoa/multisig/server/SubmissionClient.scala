package hydrozoa.multisig.server

import cats.effect.IO
import hydrozoa.multisig.consensus.{RequestSequencer, UserRequest, UserRequestBody}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.server.ApiResponse.RequestAccepted
import hydrozoa.multisig.server.JsonCodecs.given
import io.circe.Json
import io.circe.syntax.*
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.client.Client
import org.http4s.{Method, Request as Http4sRequest, Uri}

/** A client-side handle for submitting [[UserRequest]]s to a Hydrozoa peer and awaiting its
  * assigned [[RequestId]]. Abstracts over the transport: an in-process actor send, an in-memory
  * http4s round-trip against [[HydrozoaRoutes]], or a real over-the-wire HTTP call.
  */
trait SubmissionClient:
    def submit(userRequest: UserRequest): IO[RequestId]

object SubmissionClient:

    /** In-process impl that forwards to a peer's [[RequestSequencer]] actor via `?:`. Matches the
      * pre-HTTP integration path used by the multipeer harness.
      */
    def direct(handle: RequestSequencer.Handle): SubmissionClient =
        new SubmissionClient:
            def submit(userRequest: UserRequest): IO[RequestId] =
                (handle ?: userRequest).flatMap {
                    case Right(id) => IO.pure(id)
                    case Left(rejected) =>
                        IO.raiseError(new RuntimeException(s"request rejected: ${rejected.reason}"))
                }

    /** http4s-based impl: posts the request body (no header, no signature envelope — auth is the
      * native tx's own witnesses, verified at the ledger's screening) to the single submission
      * endpoint and expects a [[RequestAccepted]] JSON response. The body is externally tagged — a
      * `deposit` / `transaction` wrapper key selects the kind (see [[requestJson]]). `client` can
      * be a real http4s `Client[IO]` or an in-memory `Client.fromHttpApp` — the harness uses the
      * latter.
      */
    def http(
        client: Client[IO],
        baseUri: Uri,
    ): SubmissionClient =
        new SubmissionClient:
            def submit(userRequest: UserRequest): IO[RequestId] =
                val bodyJson = requestJson(userRequest)
                val req = Http4sRequest[IO](
                  Method.POST,
                  baseUri.withPath(Uri.Path.unsafeFromString("/head/requests"))
                ).withEntity(bodyJson)
                client.expect[RequestAccepted](req).map(_.requestId)

    private def requestJson(request: UserRequest): Json =
        request.body match
            case b: UserRequestBody.DepositRequestBody     => Json.obj("deposit" -> b.asJson)
            case b: UserRequestBody.TransactionRequestBody => Json.obj("transaction" -> b.asJson)
