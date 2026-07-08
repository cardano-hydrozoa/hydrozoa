package hydrozoa.multisig.server

import cats.effect.IO
import hydrozoa.multisig.consensus.{RequestSequencer, UserRequest}
import hydrozoa.multisig.ledger.event.RequestId

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
