package hydrozoa.integration.stage4

import cats.effect.IO
import cats.syntax.all.*
import com.suprnation.actor.ActorSystem
import hydrozoa.integration.stage4.Commands.*
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.joint.JointLedger
import org.scalacheck.commands.SutCommand

// ===================================
// Stage 4 SUT
// ===================================

/** Per-peer actor handles exposed to SUT command instances. */
case class Stage4PeerHandle(
    jointLedger: JointLedger.Handle,
)

case class Stage4Sut(
    system: ActorSystem[IO],
    cardanoBackend: CardanoBackend[IO],
    peers: Map[HeadPeerNumber, Stage4PeerHandle],
)

// ===================================
// SUT command instances (direct submission)
// ===================================

object Stage4SutCommands:

    // Delay: the framework's IO.sleep(cmd.delay) already advances simulated time under
    // TestControl. No actor message needed — delays exist only to pace model clock and
    // give the BlockWeaver time to fire its wakeup timeouts.
    given SutCommand[DelayCommand, Unit, Stage4Sut] with {
        override def run(cmd: DelayCommand, sut: Stage4Sut): IO[Unit] =
            IO.unit
    }

    // L2 tx: submit directly to the peer's JointLedger.
    // Result is always Valid (trivial); oracle check in shutdownSut compares model
    // predictions against actual block-brief outcomes.
    given SutCommand[L2TxCommand, ValidityFlag, Stage4Sut] with {
        override def run(cmd: L2TxCommand, sut: Stage4Sut): IO[ValidityFlag] =
            (sut.peers(cmd.peerNum).jointLedger ! (cmd.request: UserRequestWithId)) >>
                IO.pure(ValidityFlag.Valid)
    }

    // Deposit: register with JointLedger AND submit the signed deposit tx to the shared
    // mock L1 backend so CardanoLiaison can observe it on-chain at the correct time.
    given SutCommand[RegisterAndSubmitDepositCommand, ValidityFlag, Stage4Sut] with {
        override def run(cmd: RegisterAndSubmitDepositCommand, sut: Stage4Sut): IO[ValidityFlag] =
            (sut.peers(cmd.peerNum).jointLedger ! (cmd.request: UserRequestWithId)) >>
                sut.cardanoBackend.submitTx(cmd.depositTxBytesSigned).void >>
                IO.pure(ValidityFlag.Valid)
    }
