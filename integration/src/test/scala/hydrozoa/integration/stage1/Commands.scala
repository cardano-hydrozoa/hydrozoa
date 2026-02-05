package hydrozoa.integration.stage1

import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.LedgerEvent
import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean
import org.scalacheck.commands.CommandProp

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
            s"block briefs should be identical: \n\texpected: $expectedResult\n\tgot: $result"
}
