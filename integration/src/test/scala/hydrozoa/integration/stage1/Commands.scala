package hydrozoa.integration.stage1

import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.LedgerEvent
import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean
import org.scalacheck.commands.{CommandLabel, CommandProp}

// ===================================
// Command case classes and CommandProp instances
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

/** Start a new block in the joint ledger. */
final case class StartBlockCommand(
    blockNumber: BlockNumber,
    creationTime: QuantizedInstant
)

implicit object StartBlockCommandProp extends CommandProp[StartBlockCommand, Unit, ModelState]

/** Feed a single ledger event into the current block. */
final case class LedgerEventCommand(
    event: LedgerEvent
)

implicit object LedgerEventCommandProp extends CommandProp[LedgerEventCommand, Unit, ModelState]

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

// ===================================
// CommandLabel instances
// ===================================

implicit object DelayCommandLabel extends CommandLabel[DelayCommand]:
    def label(cmd: DelayCommand): String = cmd.delaySpec match
        case _: Delay.EndsBeforeHappyPathExpires => "Delay(happy)"
        case _: Delay.EndsInTheSilencePeriod     => "Delay(silence)"
        case _: Delay.EndsAfterHappyPathExpires  => "Delay(expired)"

implicit object StartBlockCommandLabel extends CommandLabel[StartBlockCommand]:
    def label(cmd: StartBlockCommand): String = "StartBlock"

implicit object LedgerEventCommandLabel extends CommandLabel[LedgerEventCommand]:
    def label(cmd: LedgerEventCommand): String = "LedgerEvent"

implicit object CompleteBlockCommandLabel extends CommandLabel[CompleteBlockCommand]:
    def label(cmd: CompleteBlockCommand): String =
        if cmd.isFinal then "CompleteBlock(final)" else "CompleteBlock(regular)"
