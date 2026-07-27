package hydrozoa.integration.rbr.mbt

import cats.effect.{Deferred, IO, Ref}
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.integration.rbr.mbt.Commands.DelayCommand
import hydrozoa.multisig.consensus.RequestSequencer
import org.scalacheck.commands.SutCommand

/** The running system-under-test: the multi-peer head harness plus the milestone `Deferred`s the
  * observer tracer completes as the autonomous dispute progresses.
  */
final case class Sut(
    harness: MultiPeerHeadHarness.Harness[Option[RequestSequencer.Handle]],
    fallbackDispatched: Deferred[IO, Unit],
    evacuationDone: Deferred[IO, Unit],
    firstPayoutsLeft: Ref[IO, Option[Int]],
)

/** SUT-side command execution. */
object SutCommands:

    given SutCommand[DelayCommand, Unit, Sut] with
        // The framework sleeps `ModelCommand.delay` before running; the SUT step itself is a no-op.
        override def run(cmd: DelayCommand, sut: Sut): IO[Unit] = IO.unit

end SutCommands
