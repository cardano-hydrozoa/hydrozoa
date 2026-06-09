package hydrozoa.multisig.consensus.liaison

import cats.effect.{IO, Ref}
import org.typelevel.log4cats.Logger

/** The pull half of one liaison link (§8.5 of `design/coil-network.md`): send `GetMsgBatch`es,
  * consume the remote's `NewMsgBatch` replies, and advance our inbound lane cursors. A
  * single-outstanding-request chain — each reply triggers the next request only after it verifies
  * and advances.
  *
  * Composition, not inheritance: a liaison actor *has* a `Puller` (and a [[Server]]), it does not
  * *extend* them. The per-lane heavy lifting lives in the lane primitives; the per-shape batch
  * typing lives in the actor's glue closures; this engine owns only the pull-side state machine.
  *
  * @param initialGet
  *   the first request (batch 0, initial cursors).
  * @param buildGet
  *   assemble a request with the given batch number from the current inbound cursors.
  * @param accept
  *   verify a reply against our cursors and, iff every lane matches, advance them all (atomic:
  *   `Left(reason)` leaves all cursors untouched).
  * @param dispatch
  *   route a verified reply's payloads to the local actors.
  * @param getBatchNum
  *   / [[newBatchNum]] — read the batch number off a request / reply.
  * @param send
  *   send a request to the counterpart liaison.
  */
final class Puller[G, N](
    initialGet: G,
    buildGet: BatchNumber => IO[G],
    accept: N => IO[Either[String, Unit]],
    dispatch: N => IO[Unit],
    getBatchNum: G => BatchNumber,
    newBatchNum: N => BatchNumber
)(send: G => IO[Unit])(using logger: Logger[IO]) {
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
            if newBatchNum(received) != getBatchNum(outstanding) then
                logger.debug(
                  s"dropping stale reply batch=${newBatchNum(received)} " +
                      s"(outstanding=${getBatchNum(outstanding)})"
                )
            else
                accept(received).flatMap {
                    case Left(reason) =>
                        logger.warn(s"rejecting reply batch=${newBatchNum(received)}: $reason")
                    case Right(()) =>
                        for {
                            next <- buildGet(getBatchNum(outstanding).increment)
                            _ <- currentlyRequesting.set(next)
                            _ <- dispatch(received)
                            _ <- send(next)
                        } yield ()
                }
        }
}
