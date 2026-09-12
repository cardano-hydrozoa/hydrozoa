package hydrozoa.lib

// `_root_.` because `hydrozoa.lib.cats` shadows the top-level `cats` package from here.
import _root_.cats.effect.{IO, Resource}

/** Make a resource's RELEASE non-throwing, leaving acquire and use untouched.
  *
  * ⛔ Why this exists. `JdkWSClient`'s close path hands the observed close status straight to the
  * JDK's `WebSocket.sendClose`, which rejects the RFC 6455 reserved codes 1005 (No Status Received)
  * and 1006 (Abnormal Closure) with `IllegalArgumentException: statusCode` — and those are exactly
  * the codes reported for a socket that closed WITHOUT a close handshake, i.e. every peer that
  * crashed, was killed, or vanished. Reproduced twice: once when the remote ledger was killed
  * mid-run, once at node shutdown.
  *
  * The throw escapes from a finalizer, where cats-effect can only report it to `reportFailure` and
  * drop it. That is bad in two ways: it prints a bare stack trace with no logger context (so it
  * reads like a crash), and a finalizer that throws stops being a reliable part of an orderly
  * teardown. Neither is acceptable on the shutdown path, which is precisely where a peer is most
  * likely to have gone away.
  *
  * ⚠️ Deliberately narrow: only the release is swallowed. An acquire failure still propagates — a
  * connection that cannot be opened is real news, and callers retry on it.
  */
object QuietRelease:
    def apply[A](resource: Resource[IO, A]): Resource[IO, A] =
        Resource.applyFull { poll =>
            poll(resource.allocated).map { case (a, release) =>
                (a, (_: Resource.ExitCase) => release.attempt.void)
            }
        }
