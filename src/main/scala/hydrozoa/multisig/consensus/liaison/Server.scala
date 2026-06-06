package hydrozoa.multisig.consensus.liaison

import cats.effect.{IO, Ref}
import cats.implicits.*
import hydrozoa.multisig.consensus.liaison.Server.Served

/** The serve half of one liaison link (§8.5 of `design/coil-network.md`): answer the remote's
  * `GetMsgBatch` pulls from our outbox lanes. When a pull finds nothing new, the request is stashed
  * and re-answered the moment fresh content is appended ([[afterAppend]]) — so a quiet link doesn't
  * spin.
  *
  * Composition, not inheritance: a liaison actor *has* a `Server` (and a [[Puller]]), it does not
  * *extend* them. The two halves are independent — for the asymmetric hub↔coil link they carry
  * entirely different lane sets — and share only the actor's send path.
  *
  * @param serve
  *   build a reply for a request from the current outbox lanes (echoing its batch number), or
  *   report [[Served.Empty]] / [[Served.OutOfBounds]].
  * @param send
  *   send a reply to the counterpart liaison.
  */
final class Server[G, N](
    serve: G => IO[Served[N]]
)(send: N => IO[Unit]) {

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

object Server {

    /** Outcome of serving a remote's pull — the vocabulary a liaison's `serve` closure produces and
      * a [[Server]] consumes.
      */
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
