package hydrozoa.multisig.consensus.liaison

import cats.effect.{IO, Ref}
import cats.implicits.*
import org.typelevel.log4cats.Logger

/** The shared orchestration for one liaison link, by **composition** (§8.5 of
  * `design/coil-network.md`). A link is a [[Puller]] (we pull the remote's outbox) plus a
  * [[Server]] (we serve the remote's pulls); each liaison actor owns one of each, constructing them
  * with closures over its own concrete [[Lane]]s. They are independent — for the asymmetric
  * hub↔coil link the two halves carry entirely different lane sets — and share only the actor's
  * send path.
  *
  * Neither is a base class: a liaison actor *has* a `Puller` and a `Server`, it does not *extend*
  * them. The per-lane heavy lifting lives in [[Lane]]; the per-shape batch typing lives in the
  * actor's glue closures; these two engines own only the cross-lane state machine.
  */
object BatchLink {

    /** Outcome of serving a remote's pull. */
    enum Served[+N]:
        /** The remote's cursor is ahead of anything we could have produced — protocol desync. */
        case OutOfBounds

        /** No lane has anything new for this cursor; stash the request and answer when content
          * arrives (see [[Server.afterAppend]]).
          */
        case Empty

        /** A reply to send. */
        case Reply(msg: N)
}

/** The pull half of a link: send `GetMsgBatch`es, consume the remote's `NewMsgBatch` replies, and
  * advance our inbound lane cursors. Single-outstanding-request chain — each reply triggers the
  * next request only after it verifies and advances.
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

/** The serve half of a link: answer the remote's `GetMsgBatch` pulls from our outbox lanes. When a
  * pull finds nothing new, the request is stashed and re-answered the moment fresh content is
  * appended ([[afterAppend]]) — so a quiet link doesn't spin.
  *
  * @param serve
  *   build a reply for a request from the current outbox lanes (echoing its batch number).
  * @param send
  *   send a reply to the counterpart liaison.
  */
final class Server[G, N](
    serve: G => IO[BatchLink.Served[N]]
)(send: N => IO[Unit])(using logger: Logger[IO]) {
    import BatchLink.Served

    private val sendImmediately = Ref.unsafe[IO, Option[G]](None)

    /** Answer a pull, or stash it if there is nothing new yet. */
    def handleGet(get: G): IO[Unit] =
        serve(get).flatMap {
            case Served.OutOfBounds =>
                IO.raiseError(new IllegalStateException("GetMsgBatch cursor out of bounds"))
            case Served.Empty      => sendImmediately.set(Some(get))
            case Served.Reply(msg) => send(msg)
        }

    /** Re-answer a stashed pull now that fresh content has been appended to an outbox lane. */
    def afterAppend: IO[Unit] =
        sendImmediately.getAndSet(None).flatMap(_.traverse_(handleGet))
}
