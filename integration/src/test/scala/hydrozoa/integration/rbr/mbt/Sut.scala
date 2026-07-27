package hydrozoa.integration.rbr.mbt

import cats.effect.{Deferred, IO, Ref}
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.integration.stage4.Commands.{DelayCommand, RegisterAndSubmitDepositCommand}
import hydrozoa.multisig.consensus.RequestSequencer
import hydrozoa.multisig.consensus.{UserRequest, UserRequestWithId}
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.l1.tx.RawTx
import org.scalacheck.commands.SutCommand

/** The running system-under-test: the multi-peer head harness, the milestone `Deferred`s the
  * observer completes, and `settlementFirewallArmed` — the dynamic gate the firewall consults. It
  * stays disarmed while the generated deposits settle (so their majors commit on-chain), then
  * `beforeFinalize` arms it to trip fallback.
  */
final case class Sut(
    harness: MultiPeerHeadHarness.Harness[Option[RequestSequencer.Handle]],
    fallbackDispatched: Deferred[IO, Unit],
    evacuationDone: Deferred[IO, Unit],
    firstPayoutsLeft: Ref[IO, Option[Int]],
    settlementFirewallArmed: Ref[IO, Boolean],
)

/** SUT-side command execution. */
object SutCommands:

    /** One-way lowering of a request-with-id to the submission payload (mirrors stage4). */
    extension (self: UserRequestWithId)
        private def asUserRequest: UserRequest = self match
            case UserRequestWithId.DepositRequest(_, r)     => UserRequest.DepositRequest(r.body)
            case UserRequestWithId.TransactionRequest(_, r) => UserRequest.TransactionRequest(r.body)

    given SutCommand[DelayCommand, Unit, Sut] with
        // The framework sleeps `ModelCommand.delay` before running; the SUT step itself is a no-op.
        override def run(cmd: DelayCommand, sut: Sut): IO[Unit] = IO.unit

    given SutCommand[RegisterAndSubmitDepositCommand, ValidityFlag, Sut] with
        // Submit the deposit request to the peer and put its signed deposit tx on the shared L1 —
        // the head absorbs it into L2 and commits it at the next major (settlement not yet firewalled).
        override def run(cmd: RegisterAndSubmitDepositCommand, sut: Sut): IO[ValidityFlag] =
            for
                _ <- sut.harness.peers(cmd.peerNum).submissionClient.submit(cmd.request.asUserRequest)
                _ <- sut.harness.cardanoBackend.submitTx(RawTx(cmd.depositTxBytesSigned))
            yield ValidityFlag.Valid

end SutCommands
