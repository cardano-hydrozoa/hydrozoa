package hydrozoa.multisig.server

import cats.effect.IO
import hydrozoa.multisig.consensus.{RequestSequencer, UserRequest, UserRequestBody}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.server.ApiDto.ErrorResponse
import hydrozoa.multisig.server.ApiResponse.RequestAccepted
import hydrozoa.multisig.server.JsonCodecs.given
import io.circe.Json
import io.circe.syntax.*
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.client.{Client, UnexpectedStatus}
import org.http4s.{Method, Request as Http4sRequest, Status, Uri}

/** A client-side handle for submitting [[UserRequest]]s to a Hydrozoa peer and awaiting its
  * assigned [[RequestId]]. Abstracts over the transport: an in-process actor send, an in-memory
  * http4s round-trip against [[HydrozoaRoutes]], or a real over-the-wire HTTP call.
  *
  * TODO: this does not belong in the main codebase. It is a *client* of the node, not part of one,
  * and the abstraction over transports exists for tests. What keeps it here is the packaged CLI:
  * `hydrozoa submit-deposit` and `hydrozoa submit-l2-tx` submit through [[http]]. Moving it out
  * means deciding where a first-party client lives — its own module, or collapsed into the two CLI
  * commands with the harness keeping its own. [[direct]] has no callers at all and can go with it.
  */
trait SubmissionClient:

    /** Submit, and tell an accepted request from a head that has closed its submissions. */
    def trySubmit(userRequest: UserRequest): IO[SubmissionClient.Outcome]

    /** Submit, expecting the request to be accepted: a closed head is an error here. */
    def submit(userRequest: UserRequest): IO[RequestId] =
        trySubmit(userRequest).flatMap {
            case SubmissionClient.Outcome.Accepted(id) => IO.pure(id)
            case SubmissionClient.Outcome.Closed(reason) =>
                IO.raiseError(new IllegalStateException(s"submissions closed: $reason"))
        }

object SubmissionClient:

    /** What a submission came to, short of a failure. */
    enum Outcome:
        /** The head accepted the request and assigned it `id`. */
        case Accepted(id: RequestId)

        /** The head has handed off to the rule-based regime and takes no more requests: the
          * submission endpoint's `503` carrying [[HydrozoaRoutes.SubmissionsClosed]]. Expected at
          * and after a fallback, so a result rather than an error.
          */
        case Closed(reason: String)

    /** In-process impl that forwards to a peer's [[RequestSequencer]] actor via `?:`. Matches the
      * pre-HTTP integration path used by the multipeer harness.
      */
    def direct(handle: RequestSequencer.Handle): SubmissionClient =
        new SubmissionClient:
            def trySubmit(userRequest: UserRequest): IO[Outcome] =
                (handle ?: userRequest).flatMap {
                    case Right(id) => IO.pure(Outcome.Accepted(id))
                    case Left(rejected) =>
                        IO.raiseError(new RuntimeException(s"request rejected: ${rejected.reason}"))
                }

    /** http4s-based impl: posts the request body (no header, no signature envelope — auth is the
      * native tx's own witnesses, verified at the ledger's screening) to the single submission
      * endpoint and expects a [[RequestAccepted]] JSON response. The body is internally tagged — a
      * `type` field (`deposit` / `transaction`) selects the kind, with the payloads alongside it
      * (see [[requestJson]]). `client` can be a real http4s `Client[IO]` or an in-memory
      * `Client.fromHttpApp` — the harness uses the latter.
      */
    def http(
        client: Client[IO],
        baseUri: Uri,
    ): SubmissionClient =
        new SubmissionClient:
            def trySubmit(userRequest: UserRequest): IO[Outcome] =
                val bodyJson = requestJson(userRequest)
                val req = Http4sRequest[IO](
                  Method.POST,
                  baseUri.withPath(Uri.Path.unsafeFromString("/head/requests"))
                ).withEntity(bodyJson)
                // Any status but a success or the closed-submissions 503 raises, as `expect` did.
                def unexpected(status: Status): IO[Outcome] =
                    IO.raiseError(UnexpectedStatus(status, req.method, req.uri))
                client.run(req).use { resp =>
                    if resp.status.isSuccess then
                        resp.as[RequestAccepted].map(a => Outcome.Accepted(a.requestId))
                    else if resp.status == Status.ServiceUnavailable then
                        resp.attemptAs[ErrorResponse].value.flatMap {
                            case Right(ErrorResponse(reason))
                                if reason == HydrozoaRoutes.SubmissionsClosed =>
                                IO.pure(Outcome.Closed(reason))
                            case _ => unexpected(resp.status)
                        }
                    else unexpected(resp.status)
                }

    /** The submission body: the kind tag, the payloads, and the digest the submitter computed over
      * them. The head re-derives the digest and refuses the request on a mismatch, so this is the
      * one field a client cannot copy from somewhere else — see `docs/user-guide/REQUEST-HASH.md`.
      */
    private def requestJson(request: UserRequest): Json =
        val (tag, body) = request.body match
            case b: UserRequestBody.DepositRequestBody     => ("deposit", b.asJson)
            case b: UserRequestBody.TransactionRequestBody => ("transaction", b.asJson)
        Json
            .obj(
              "type" -> Json.fromString(tag),
              "requestHash" -> Json.fromString(request.requestHash.toHex)
            )
            .deepMerge(body)
