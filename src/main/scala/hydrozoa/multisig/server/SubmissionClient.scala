package hydrozoa.multisig.server

import cats.effect.IO
import hydrozoa.lib.cardano.wallet.Cip30SignedData
import hydrozoa.multisig.consensus.peer.PeerWallet
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
                handle ?: userRequest

    /** http4s-based impl: signs the [[UserRequestHeader]] with `wallet` as a CIP-30 COSE_Sign1,
      * routes the request to the deposit-register or l2-submit endpoint by variant, and expects a
      * [[RequestAccepted]] JSON response. `client` can be a real http4s `Client[IO]` or an
      * in-memory `Client.fromHttpApp` — the harness uses the latter so no socket is bound.
      */
    def http(
        client: Client[IO],
        baseUri: Uri,
        wallet: PeerWallet,
    ): SubmissionClient =
        new SubmissionClient:
            def submit(userRequest: UserRequest): IO[RequestId] =
                val signed = wallet.signCoseCip30(userRequest.header.bytes)
                val bodyJson = signedRequestJson(userRequest, signed)
                val path = pathFor(userRequest)
                val req = Http4sRequest[IO](Method.POST, baseUri.withPath(path))
                    .withEntity(bodyJson)
                client.expect[RequestAccepted](req).map(_.requestId)

    private def pathFor(request: UserRequest): Uri.Path =
        request match
            case _: UserRequest.DepositRequest => Uri.Path.unsafeFromString("/api/deposit/register")
            case _: UserRequest.TransactionRequest => Uri.Path.unsafeFromString("/api/l2/submit")

    private def signedRequestJson(
        request: UserRequest,
        signed: Cip30SignedData,
    ): Json =
        val bodyField: (String, Json) = request.body match
            case b: UserRequestBody.DepositRequestBody     => "deposit" -> b.asJson
            case b: UserRequestBody.TransactionRequestBody => "transaction" -> b.asJson
        Json.obj(
          "header" -> request.header.asJson,
          bodyField,
          "coseKey" -> Json.fromString(signed.coseKeyCborHex),
          "coseSignature" -> Json.fromString(signed.coseSignatureCborHex),
        )
