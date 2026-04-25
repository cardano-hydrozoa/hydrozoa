package hydrozoa.integration.stage4

import cats.effect.{Fiber, IO, Ref}
import cats.syntax.all.*
import com.suprnation.actor.ActorSystem
import hydrozoa.integration.stage4.Commands.*
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.{BlockWeaver, CardanoLiaison, ConsensusActor, EventSequencer, PeerLiaison, UserRequest, UserRequestWithId}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.joint.JointLedger
import org.scalacheck.commands.SutCommand

// ===================================
// Per-peer actor stack (local to startupSut)
// ===================================

private[stage4] case class PeerStack(
    blockWeaver: BlockWeaver.Handle,
    cardanoLiaison: CardanoLiaison.Handle,
    eventSequencer: EventSequencer.Handle,
    jointLedger: JointLedger.Handle,
    consensusActor: ConsensusActor.Handle,
)

// ===================================
// Stage 4 SUT
// ===================================

/** Per-peer actor handles exposed to SUT commands. */
case class Stage4PeerHandle(
    eventSequencer: EventSequencer.Handle,
)

case class Stage4Sut(
    system: ActorSystem[IO],
    cardanoBackend: CardanoBackend[IO],
    peers: Map[HeadPeerNumber, Stage4PeerHandle],
    sutErrors: Ref[IO, List[String]],
    errorDrainer: Fiber[IO, Throwable, Nothing],
)

// ===================================
// SUT command instances (direct submission)
// ===================================

object Stage4SutCommands:

    private val logger = Logging.loggerIO("Stage4.Sut")

    // Delay: the framework's IO.sleep(cmd.delay) already advances simulated time under
    // TestControl. No actor message needed — delays exist only to pace model clock and
    // give the BlockWeaver time to fire its wakeup timeouts.
    given SutCommand[DelayCommand, Unit, Stage4Sut] with {
        override def run(cmd: DelayCommand, sut: Stage4Sut): IO[Unit] = {
            // TODO: shall we sleep here?
            IO.unit
        }
    }

    // L2 tx: submit directly to the peer's JointLedger.
    // Result is always Valid (trivial); oracle check in shutdownSut compares model
    // predictions against actual block-brief outcomes.
    // TODO: why validity flag here?
    given SutCommand[L2TxCommand, ValidityFlag, Stage4Sut] with {
        override def run(cmd: L2TxCommand, sut: Stage4Sut): IO[ValidityFlag] = for {
            reqId <- sut.peers(cmd.peerNum).eventSequencer ?: cmd.request.asUserRequest
            _ <- logger.trace(s"reqId=$reqId, cmd.request.requestId=${cmd.request.requestId}")
        } yield ValidityFlag.Valid
    }

    // Deposit: register with JointLedger AND submit the signed deposit tx to the shared
    // mock L1 backend so CardanoLiaison can observe it on-chain at the correct time.
    given SutCommand[RegisterAndSubmitDepositCommand, ValidityFlag, Stage4Sut] with {
        override def run(cmd: RegisterAndSubmitDepositCommand, sut: Stage4Sut): IO[ValidityFlag] = for {
            reqId <- sut.peers(cmd.peerNum).eventSequencer ?: cmd.request.asUserRequest
            _ <- logger.trace(s"reqId=$reqId, cmd.request.requestId=${cmd.request.requestId}")
            _ <-  sut.cardanoBackend.submitTx(cmd.depositTxBytesSigned)
        } yield ValidityFlag.Valid
    }

extension (self: UserRequestWithId)

    /** One-way loosing conversion
      * @return
      */
    def asUserRequest: UserRequest = self match {
        case UserRequestWithId.DepositRequest(_, r) =>
            UserRequest.DepositRequest(
              header = r.header,
              body = r.body,
              userVk = r.userVk
            )
        case UserRequestWithId.TransactionRequest(_, r) =>
            UserRequest.TransactionRequest(
              header = r.header,
              body = r.body,
              userVk = r.userVk
            )
    }
