package hydrozoa.integration.rbr.mbt

import cats.effect.{Deferred, IO, Ref}
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.integration.stage4.Commands.{DelayCommand, RegisterAndSubmitDepositCommand}
import hydrozoa.multisig.consensus.{RequestSequencer, UserRequest, UserRequestWithId}
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.l1.tx.RawTx
import org.scalacheck.commands.SutCommand
import scalus.cardano.ledger.TransactionInput

/** The running system-under-test: the multi-peer head harness, the milestone `Deferred`s the
  * observer completes, and `settlementFirewallArmed` — the dynamic gate the firewall consults. It
  * stays disarmed while the generated deposits settle (so their majors commit on-chain), then
  * `beforeFinalize` arms it to trip fallback.
  *
  * `submittedSettlementInputs` accumulates the L1 inputs of every settlement the SUT actually put
  * on the wire (observed at the firewall — a submitted settlement, not a mere ledger decision to
  * absorb). Crossed against the L1 snapshot at fallback it tells `beforeFinalize` which deposits
  * were genuinely committed (spent by a landed settlement) versus left pending for the
  * cardano-liaison refund path — so the model can agree with the SUT on each deposit's fate without
  * racing "peer submitted the tx" against "the tx appears on chain".
  *
  * `committedMaps` accumulates every `StackComposer.CommittedMap` peer trace — the `(version, size)`
  * of each committed evacuation map. `settledMajors` accumulates the majors of every settlement that
  * cleared the firewall (i.e. settled on-chain). Together they let `beforeFinalize` read the
  * committed map size the head resolves to under the last on-chain major `M` directly from the peer
  * traces, instead of reconstructing it from the model's deposit accounting.
  */
final case class Sut(
    harness: MultiPeerHeadHarness.Harness[Option[RequestSequencer.Handle]],
    fallbackDispatched: Deferred[IO, Unit],
    evacuationDone: Deferred[IO, Unit],
    firstPayoutsLeft: Ref[IO, Option[Int]],
    settlementFirewallArmed: Ref[IO, Boolean],
    submittedSettlementInputs: Ref[IO, Set[TransactionInput]],
    committedMaps: Ref[IO, List[(BlockVersion.Full, Int)]],
    settledMajors: Ref[IO, Set[Int]],
)

/** SUT-side command execution. */
object SutCommands:

    /** One-way lowering of a request-with-id to the submission payload (mirrors stage4). */
    extension (self: UserRequestWithId)
        private def asUserRequest: UserRequest = self match
            case UserRequestWithId.DepositRequest(_, r) => UserRequest.DepositRequest(r.body)
            case UserRequestWithId.TransactionRequest(_, r) =>
                UserRequest.TransactionRequest(r.body)

    given SutCommand[DelayCommand, Unit, Sut] with
        // The framework sleeps `ModelCommand.delay` before running; the SUT step itself is a no-op.
        override def run(cmd: DelayCommand, sut: Sut): IO[Unit] = IO.unit

    given SutCommand[RegisterAndSubmitDepositCommand, ValidityFlag, Sut] with
        // Submit the deposit request to the peer and put its signed deposit tx on the shared L1 —
        // the head absorbs it into L2 and commits it at the next major (settlement not yet firewalled).
        override def run(cmd: RegisterAndSubmitDepositCommand, sut: Sut): IO[ValidityFlag] =
            for
                _ <- sut.harness
                    .peers(cmd.peerNum)
                    .submissionClient
                    .submit(cmd.request.asUserRequest)
                _ <- sut.harness.cardanoBackend.submitTx(RawTx(cmd.depositTxBytesSigned))
            yield ValidityFlag.Valid

end SutCommands
