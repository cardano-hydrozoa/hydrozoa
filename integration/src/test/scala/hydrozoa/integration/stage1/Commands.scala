package hydrozoa.integration.stage1

import hydrozoa.integration.stage1.Generators.{TxMutator, TxStrategy}
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.{LedgerEvent, LedgerEventId}
import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean
import org.scalacheck.commands.{CommandLabel, CommandProp}

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

implicit object DelayCommandProp extends CommandProp[DelayCommand, Unit, ModelState]

implicit object DelayCommandLabel extends CommandLabel[DelayCommand]:
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

implicit object StartBlockCommandProp extends CommandProp[StartBlockCommand, Unit, ModelState]

implicit object StartBlockCommandLabel extends CommandLabel[StartBlockCommand]:
    override def label(cmd: StartBlockCommand): String = "StartBlock"

// ===================================
// L2 Transaction
// ===================================

/** Feed a single L2 transaction into the current block. */
final case class L2TxCommand(
    event: LedgerEvent.TxL2Event,
    txStrategy: TxStrategy,
    txMutator: TxMutator
)

implicit object L2TxCommandProp extends CommandProp[L2TxCommand, Unit, ModelState]

implicit object L2TxCommandLabel extends CommandLabel[L2TxCommand]:
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

/** Postcondition for [[CompleteBlockCommand]]: verifies model and SUT agree on the block brief. */
implicit object CompleteBlockCommandProp
    extends CommandProp[CompleteBlockCommand, BlockBrief, ModelState] {

    override def onSuccessCheck(
        cmd: CompleteBlockCommand,
        expectedResult: BlockBrief,
        stateBefore: ModelState,
        stateAfter: ModelState,
        result: BlockBrief
    ): Prop =
        (expectedResult == result) :|
            "block briefs should be identical: " +
            s"\n\texpected: $expectedResult" +
            s"\n\tgot: $result"
}

implicit object CompleteBlockCommandLabel extends CommandLabel[CompleteBlockCommand]:
    override def label(cmd: CompleteBlockCommand): String =
        if cmd.isFinal then "CompleteBlock(final)" else "CompleteBlock(regular)"

// ===================================
// Deposit Request Command
// ===================================

final case class RegisterDepositCommand(
    registerDeposit: LedgerEvent.RegisterDeposit
)

// ===================================
// Submit Deposit Command
// ===================================

final case class SubmitDepositCommand(
    depositEventId: LedgerEventId
)
