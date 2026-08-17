package hydrozoa.multisig.consensus.liaison

import cats.effect.{IO, Ref}
import hydrozoa.lib.logging.ContraTracer

/** The pull half of one liaison link (§5.5 of `docs/spec/coil-network.md`) [doc-ref]: send
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
  *   supplies the starting batch number and seeds the outstanding-request `Ref` before the first
  *   pull; [[start]] rebuilds its cursors from the (possibly recovery-restored) live lanes.
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
    tracer: ContraTracer[IO, PeerLiaisonEvent],
    // Human summaries for the batch-traffic events; default to empty so links that don't care
    // (e.g. the coil hard-ack lane) need not supply them.
    describeGet: G => String = (_: G) => "",
    describeBatch: N => String = (_: N) => ""
)(send: G => IO[Unit]) {
    private val currentlyRequesting = Ref.unsafe[IO, G](initialGet)

    /** Send a pull, tracing it as a `BatchRequested` (covers initial, retransmit, and next). */
    private def sendTraced(g: G): IO[Unit] =
        tracer.traceWith(
          PeerLiaisonEvent.BatchRequested(numberOfBatchRequest(g), describeGet(g))
        ) >> send(g)

    /** Send the initial request, rebuilt from the current inbound cursors. After a crash the lanes
      * have been restored to `next(max received)`, so the first pull must carry those restored
      * cursors — not the cold [[initialGet]], which would re-pull the whole history and be rejected
      * by our own `verify` as a stale re-serve (a lane already past that number), looping forever
      * on the retransmit tick. On a cold boot the lanes still sit at their initial cursors, so this
      * reproduces [[initialGet]] exactly. The batch number stays [[initialGet]]'s.
      */
    def start: IO[Unit] =
        buildGet(numberOfBatchRequest(initialGet)).flatMap(g =>
            currentlyRequesting.set(g) >> sendTraced(g)
        )

    /** Re-send the outstanding request — the retransmit tick that self-heals the chain after a
      * wire-level loss.
      */
    def resend: IO[Unit] = currentlyRequesting.get.flatMap(sendTraced)

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
                            _ <- tracer.traceWith(
                              PeerLiaisonEvent.BatchReceived(
                                numberOfBatch(received),
                                describeBatch(received)
                              )
                            )
                            next <- buildGet(numberOfBatchRequest(outstanding).increment)
                            _ <- currentlyRequesting.set(next)
                            _ <- dispatch(received)
                            _ <- sendTraced(next)
                        } yield ()
                }
        }
}
