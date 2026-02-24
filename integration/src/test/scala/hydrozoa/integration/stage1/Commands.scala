package hydrozoa.integration.stage1

import hydrozoa.integration.stage1.CommandGen.{TxMutator, TxStrategy}
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.dapp.txseq.DepositRefundTxSeq
import hydrozoa.multisig.ledger.event.{LedgerEvent, LedgerEventId}
import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean
import org.scalacheck.commands.{CommandLabel, CommandProp}
import scalus.cardano.ledger.Transaction

object Commands:

    private val logger = Logging.logger("Stage1.Commands")

    // ===================================
    // Delay
    // ===================================

    /** Advance time with three possible outcomes, transitioning the model's [[CurrentTime]].
      */
    final case class DelayCommand(
        delaySpec: Delay
    )

    /** Which timing region the delay lands in, used to drive the [[CurrentTime]] transition in the
      * model.
      */
    enum Delay(d: QuantizedFiniteDuration):
        case EndsBeforeHappyPathExpires(d: QuantizedFiniteDuration) extends Delay(d)
        case EndsInTheSilencePeriod(d: QuantizedFiniteDuration) extends Delay(d)
        case EndsAfterHappyPathExpires(d: QuantizedFiniteDuration) extends Delay(d)

        def duration: QuantizedFiniteDuration = d

    implicit given CommandProp[DelayCommand, Unit, Model.State] with {}

    implicit given CommandLabel[DelayCommand] with
        override def label(cmd: DelayCommand): String = cmd.delaySpec match
            case _: Delay.EndsBeforeHappyPathExpires => "Delay(happy)"
            case _: Delay.EndsInTheSilencePeriod     => "Delay(silence)"
            case _: Delay.EndsAfterHappyPathExpires  => "Delay(expired)"

    // ===================================
    // Start Block
    // ===================================

    /** Start a new block in the joint ledger. */
    final case class StartBlockCommand(
        blockNumber: BlockNumber,
        creationTime: QuantizedInstant
    )

    implicit given CommandProp[StartBlockCommand, Unit, Model.State] with {}

    implicit given CommandLabel[StartBlockCommand] with
        override def label(cmd: StartBlockCommand): String = "StartBlock"

    // ===================================
    // L2 Transaction
    // ===================================

    /** Feed a single L2 transaction into the current block. */
    final case class L2TxCommand(
        event: LedgerEvent.L2TxEvent,
        txStrategy: TxStrategy,
        txMutator: TxMutator
    )

    implicit given CommandProp[L2TxCommand, Unit, Model.State] with {}

    implicit given CommandLabel[L2TxCommand] with
        override def label(cmd: L2TxCommand): String = cmd.txStrategy match {
            case TxStrategy.Arbitrary =>
                cmd.txMutator match {
                    case TxMutator.Identity      => "L2Tx(arbitrary, identity)"
                    case TxMutator.DropWitnesses => "L2Tx(arbitrary, drop witnesses)"
                }
            case TxStrategy.Regular =>
                cmd.txMutator match {
                    case TxMutator.Identity      => "L2Tx(regular, identity)"
                    case TxMutator.DropWitnesses => "L2Tx(regular, drop witnesses)"
                }
            case TxStrategy.RandomWithdrawals =>
                cmd.txMutator match {
                    case TxMutator.Identity      => "L2Tx(random withdrawals, identity)"
                    case TxMutator.DropWitnesses => "L2Tx(random withdrawals, drop witnesses)"
                }
            case TxStrategy.Dust(maxOutputs) =>
                cmd.txMutator match {
                    case TxMutator.Identity      => s"L2Tx(dust=$maxOutputs, identity)"
                    case TxMutator.DropWitnesses => s"L2Tx(dust=$maxOutputs, drop witnesses)"
                }

        }

    // ===================================
    // Complete Block
    // ===================================

    /** Complete the current block (regular or final).  Result is the [[BlockBrief]] produced. */
    final case class CompleteBlockCommand(
        blockNumber: BlockNumber,
        isFinal: Boolean
    )

    /** Postcondition for [[CompleteBlockCommand]]: verifies model and SUT agree on the block brief.
      */
    implicit given CommandProp[CompleteBlockCommand, BlockBrief, Model.State] with

        override def onSuccessCheck(
            cmd: CompleteBlockCommand,
            expectedResult: BlockBrief,
            stateBefore: Model.State,
            stateAfter: Model.State,
            result: BlockBrief
        ): Prop =
            logger.trace(s"expected result: $expectedResult")
            logger.trace(s"actual result: $result")

            (expectedResult == result) :|
                "block briefs should be identical: " +
                s"\n\texpected: $expectedResult" +
                s"\n\tgot: $result"

    implicit given CommandLabel[CompleteBlockCommand] with
        override def label(cmd: CompleteBlockCommand): String =
            if cmd.isFinal then "CompleteBlock(final)" else "CompleteBlock(regular)"

    // ===================================
    // Deposit Request Command
    // ===================================

    /** The command corresponds to the register deposit action with the event id known upfront.
      */
    final case class RegisterDepositCommand(
        registerDeposit: LedgerEvent.DepositEvent,
        depositRefundTxSeq: DepositRefundTxSeq,
        depositTxBytesSigned: Transaction
    ) {
        override def toString: String = s"RegisterDepositCommand: ${registerDeposit.eventId}"
    }

    implicit given CommandProp[RegisterDepositCommand, Unit, Model.State] with {}

    implicit given CommandLabel[RegisterDepositCommand] with
        override def label(cmd: RegisterDepositCommand): String = "Register deposit"

    // ===================================
    // Submit Deposit Command
    // ===================================

    /** The command submits the deposit transaction from the corresponding register deposit event.
      */
    final case class SubmitDepositCommand(
        depositEventIds: List[LedgerEventId]
    )

    implicit given CommandProp[SubmitDepositCommand, Unit, Model.State] with {}

    implicit given CommandLabel[SubmitDepositCommand] with
        override def label(cmd: SubmitDepositCommand): String =
            s"Submit deposits (n=${cmd.depositEventIds.size})"

end Commands
