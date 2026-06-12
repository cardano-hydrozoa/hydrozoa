package hydrozoa.multisig.consensus.liaison

import cats.effect.{IO, Ref}
import hydrozoa.lib.logging.ContraTracer

/** The pull half of one liaison link (§5.5 of `design/coil-network.md`) [doc-ref]: send
  * `GetMsgBatch`es, consume the remote's `NewMsgBatch` replies, and advance our inbound lane
  * cursors. A single-outstanding-request chain — each reply triggers the next request only after it
  * verifies and advances.
  *
  * Composition, not inheritance: a liaison actor *has* a `Puller` (and a [[Server]]), it does not
  * *extend* them. The lane primitives do the per-lane heavy lifting; the actor's glue closures fix
  * the batch types for its link; this engine owns only the pull-side state machine.
  *
  * @tparam G
  *   the type of batch request (a `GetMsgBatch` of next-expected cursors).
  * @tparam N
  *   the type of batch (a `NewMsgBatch` of payload slices).
  * @param initialGet
  *   the first request (batch 0, initial cursors).
  * @param buildGet
  *   assemble a request with the given batch number from the current inbound cursors.
  * @param accept
  *   verify a reply against our cursors and, iff every lane matches, advance them all (atomic:
  *   `Left(reason)` leaves all cursors untouched).
  * @param dispatch
  *   route a verified reply's payloads to the local actors.
  * @param numberOfBatchRequest
  *   read the batch number off a batch request
  * @param numberOfBatch
  *   read the batch number off a batch
  * @param tracer
  *   the owning liaison's event channel; emits the stale-drop / reject events.
  * @param send
  *   send a request to the counterpart liaison.
  */
final class Puller[G, N](
    initialGet: G,
    buildGet: BatchNumber => IO[G],
    accept: N => IO[Either[String, Unit]],
    dispatch: N => IO[Unit],
    numberOfBatchRequest: G => BatchNumber,
    numberOfBatch: N => BatchNumber,
    tracer: ContraTracer[IO, PeerLiaisonEvent]
)(send: G => IO[Unit]) {
    private val currentlyRequesting = Ref.unsafe[IO, G](initialGet)

    /** Send the initial request. */
    def start: IO[Unit] = currentlyRequesting.set(initialGet) >> send(initialGet)

    /** Re-send the outstanding request — the retransmit tick that self-heals the chain after a
      * wire-level loss.
      */
    def resend: IO[Unit] = currentlyRequesting.get.flatMap(send)

    /** Handle a reply: drop stale duplicates (wrong batch number), reject on verify failure (the
      * retransmit tick keeps the chain alive), or advance + dispatch + request the next batch.
      */
    def handleReply(received: N): IO[Unit] =
        currentlyRequesting.get.flatMap { outstanding =>
            if numberOfBatch(received) != numberOfBatchRequest(outstanding) then
                tracer.traceWith(
                  PeerLiaisonEvent.StaleBatchDropped(
                    numberOfBatch(received),
                    numberOfBatchRequest(outstanding)
                  )
                )
            else
                accept(received).flatMap {
                    case Left(reason) =>
                        tracer.traceWith(
                          PeerLiaisonEvent.BatchRejected(numberOfBatch(received), reason)
                        )
                    case Right(()) =>
                        for {
                            next <- buildGet(numberOfBatchRequest(outstanding).increment)
                            _ <- currentlyRequesting.set(next)
                            _ <- dispatch(received)
                            _ <- send(next)
                        } yield ()
                }
        }
}
