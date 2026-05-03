package hydrozoa.integration.stage4

import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.l1.txseq.DepositRefundTxSeq
import org.scalacheck.commands.{CommandLabel, CommandProp}
import scalus.cardano.ledger.{Transaction, TransactionInput}
import scalus.uplc.builtin.ByteString

import scala.concurrent.duration.FiniteDuration

object Commands:

    // ===================================
    // Delay
    // ===================================

    /** Advance model time for a specific peer. In execution, becomes IO.sleep on that peer's fiber.
      */
    final case class DelayCommand(
        peerNum: HeadPeerNumber,
        duration: QuantizedFiniteDuration
    ) {
        override def toString: String = s"DelayCommand(peer=$peerNum, duration=$duration)"
    }

    given CommandProp[DelayCommand, Unit, Model.ModelState] with {}

    given CommandLabel[DelayCommand] with
        override def label(cmd: DelayCommand): String = s"Delay(peer=${cmd.peerNum: Int})"

    // ===================================
    // L2 Transaction
    // ===================================

    enum TxMutator:
        case Identity
        case DropWitnesses

    enum TxStrategy:
        case Regular
        case Arbitrary
        case RandomWithdrawals
        case Dust(maxOutputs: Int)

    /** Submit an L2 transaction to the given peer. Result is the model's validity prediction; the
      * CommandProp is trivially passed (oracle check done in shutdownSut).
      */
    final case class L2TxCommand(
        peerNum: HeadPeerNumber,
        request: UserRequestWithId.TransactionRequest,
        txStrategy: TxStrategy,
        txMutator: TxMutator,
        // Sampled from Exp(1/μ) at generation time. Returned by ModelCommand.delay so the SUT
        // fiber sleeps for this duration before issuing the command; model clock advances by the
        // same amount in runState, keeping model and wall time in sync.
        interArrivalDelay: FiniteDuration,
    ) {
        override def toString: String =
            s"L2TxCommand(peer=$peerNum, requestId=${request.requestId}, " +
                s"strategy=$txStrategy, mutator=$txMutator)"
    }

    given CommandProp[L2TxCommand, ValidityFlag, Model.ModelState] with {}

    given CommandLabel[L2TxCommand] with
        override def label(cmd: L2TxCommand): String =
            val mutStr = cmd.txMutator match
                case TxMutator.Identity      => "identity"
                case TxMutator.DropWitnesses => "drop-witnesses"
            val stratStr = cmd.txStrategy match
                case TxStrategy.Regular          => "regular"
                case TxStrategy.Arbitrary        => "arbitrary"
                case TxStrategy.RandomWithdrawals => "withdrawals"
                case TxStrategy.Dust(n)          => s"dust=$n"
            s"L2Tx(peer=${cmd.peerNum: Int}, $stratStr, $mutStr)"

    // ===================================
    // Register Deposit
    // ===================================

    /** Register a deposit with the given peer and immediately submit the deposit tx to L1.
      *  Result is the model's validity prediction (always Valid — invalid deposits are tested in stage1).
      */
    final case class RegisterAndSubmitDepositCommand(
        peerNum: HeadPeerNumber,
        request: UserRequestWithId.DepositRequest,
        l2Payload: ByteString,
        depositProduced: TransactionInput,
        depositTxBytesSigned: Transaction,
        interArrivalDelay: FiniteDuration,
        // Pre-computed at generation time as depositAbsorptionStartTime(validityEnd) + absorptionSlack.
        // Stored here so runState doesn't need to re-derive it from txTiming, and so the table
        // can display the expected absorption time alongside the deposit command.
        expectedAbsorptionTime: QuantizedInstant,
    ) {
        override def toString: String =
            s"RegisterAndSubmitDepositCommand(peer=$peerNum, requestId=${request.requestId})"
    }

    given CommandProp[RegisterAndSubmitDepositCommand, ValidityFlag, Model.ModelState] with {}

    given CommandLabel[RegisterAndSubmitDepositCommand] with
        override def label(cmd: RegisterAndSubmitDepositCommand): String =
            s"RegisterDeposit(peer=${cmd.peerNum: Int}, abs=${cmd.expectedAbsorptionTime.getEpochSecond})"

end Commands
