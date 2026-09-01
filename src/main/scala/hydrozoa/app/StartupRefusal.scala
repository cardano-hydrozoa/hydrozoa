package hydrozoa.app

import cats.effect.{ExitCode, IO}

/** A deliberate, deterministic refusal to start: something about THIS node is wrong, and starting
  * it again will reach the same conclusion.
  *
  * ⛔ The distinction this carries is the whole point. A node that cannot reach Cardano, its ledger
  * or its peers now **waits** — indefinitely, visibly, and without exiting — because the world may
  * yet become ready. A node whose config is malformed, whose script reference UTxOs are not on
  * chain, or whose store is held by another instance is in a state no amount of retrying can fix,
  * and it must fail loudly and STAY failed.
  *
  * Conflating the two is what produces an infinite `Restart=on-failure` loop: the supervisor
  * answers a permanent verdict by re-deriving it every `RestartSec`, forever, learning nothing. So
  * a refusal exits with [[StartupRefusal.exitCode]] and the unit sets `RestartPreventExitStatus=`
  * to match.
  *
  * ⚠️ Do NOT reach for this to make an unavailable dependency fail faster. If a human could fix it
  * by waiting, it is not a refusal.
  */
final case class StartupRefusal(reason: String, cause: Option[Throwable] = None)
    extends RuntimeException(reason, cause.orNull)

object StartupRefusal:

    /** Distinct from 1 (a generic failure) so a supervisor can tell "do not restart me" from "I
      * crashed and a restart might help". Mirrored by `RestartPreventExitStatus=2` in the unit.
      */
    val exitCode: ExitCode = ExitCode(2)

    /** Run `io`, turning a refusal into [[exitCode]] rather than an unhandled crash. Anything else
      * propagates unchanged: an unexpected throwable is not a considered verdict and a restart may
      * well be the right answer to it.
      */
    def guard(io: IO[ExitCode]): IO[ExitCode] =
        io.handleErrorWith {
            case r: StartupRefusal =>
                IO.println(
                  s"hydrozoa refuses to start: ${r.reason}\n" +
                      "This is a deliberate refusal, not a transient failure — restarting will " +
                      "reach the same conclusion. Fix the cause and start again."
                ).as(exitCode)
            case other => IO.raiseError(other)
        }
