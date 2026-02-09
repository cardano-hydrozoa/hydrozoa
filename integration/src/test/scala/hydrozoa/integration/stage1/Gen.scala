package hydrozoa.integration.stage1

import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.lib.cardano.scalus.given_Choose_QuantizedInstant
import hydrozoa.multisig.ledger.block.BlockNumber
import org.scalacheck.commands.{AnyCommand, CommandGen, noOp}
import org.scalacheck.{Arbitrary, Gen}
import scalus.cardano.ledger.SlotConfig
import test.Generators.Hydrozoa.ArbitraryInstances.given_Arbitrary_LedgerEvent

// ===================================
// Per-command generators
// ===================================

/** Per-command generators. These produce concrete command values; the caller wraps each into
  * [[AnyCommand]] where the [[org.scalacheck.commands.SutCommand]] implicit is in scope.
  */
object Generators:

    def genDelay(
        currentTime: QuantizedInstant,
        settlementExpirationTime: QuantizedInstant,
        competingFallbackStartTime: QuantizedInstant,
        slotConfig: SlotConfig
    ): Gen[DelayCommand] = for {
        delay <- Gen
            .frequency(
              50 -> Gen
                  .choose(currentTime, settlementExpirationTime)
                  .flatMap(d => DelayCommand(Delay.EndsBeforeHappyPathExpires(d - currentTime))),
              1 -> Gen
                  .choose(settlementExpirationTime, competingFallbackStartTime)
                  .flatMap(d => DelayCommand(Delay.EndsInTheSilencePeriod(d - currentTime))),
              1 ->
                  Gen
                      .choose(
                        competingFallbackStartTime,
                        competingFallbackStartTime + QuantizedFiniteDuration(
                          slotConfig,
                          (competingFallbackStartTime - currentTime).finiteDuration / 10
                        )
                      )
                      .flatMap(d => DelayCommand(Delay.EndsInTheSilencePeriod(d - currentTime))),
            )
    } yield delay

    def genStartBlock(
        prevBlockNumber: BlockNumber,
        currentTime: QuantizedInstant
    ): Gen[StartBlockCommand] =
        Gen.const(
          StartBlockCommand(
            prevBlockNumber.increment,
            currentTime
          )
        )

    def genArbitraryLedgerEvent: Gen[LedgerEventCommand] = for {
        event <- Arbitrary.arbitrary[hydrozoa.multisig.ledger.event.LedgerEvent]
    } yield LedgerEventCommand(event)

    def genCompleteBlock(blockNumber: BlockNumber): Gen[CompleteBlockCommand] = for {
        isFinal <- Gen.frequency(
          1 -> true,
          40 -> false
        )
    } yield CompleteBlockCommand(blockNumber, isFinal)

// ===================================
// Default command generator
// ===================================

/** There is a customizable delay before starting every new block. If the delay happens to be long
  * enough so the latest fallback becomes active, all next commands are NoOp and the fallback is
  * expected to be submitted. Otherwise, only happy path effects are expected to be submitted.
  */
object ArbitraryEventsOnly extends CommandGen[ModelState, Stage1Sut]:

    override def genNextCommand(
        state: ModelState
    ): Gen[AnyCommand[ModelState, Stage1Sut]] = {
        import hydrozoa.integration.stage1.BlockCycle.*
        import hydrozoa.integration.stage1.CurrentTime.BeforeHappyPathExpiration

        state.currentTime match {
            case BeforeHappyPathExpiration(_) =>
                state.blockCycle match {
                    case Done(blockNumber, _) =>
                        val settlementExpirationTime =
                            state.txTiming.currentSettlementExpiringTime(
                              state.competingFallbackStartTime
                            )
                        Generators
                            .genDelay(
                              state.currentTime.instant,
                              settlementExpirationTime,
                              state.competingFallbackStartTime,
                              state.cardanoInfo.slotConfig
                            )
                            .map(AnyCommand(_))

                    case Ready(blockNumber, _) =>
                        Generators
                            .genStartBlock(blockNumber, state.currentTime.instant)
                            .map(AnyCommand(_))

                    case InProgress(blockNumber, _, _) =>
                        Gen.frequency(
                          1 -> Generators
                              .genCompleteBlock(blockNumber)
                              .map(AnyCommand(_)),
                          10 -> Generators.genArbitraryLedgerEvent
                              .map(AnyCommand(_))
                        )

                    case HeadFinalized => Gen.const(noOp)
                }
            case _ => Gen.const(noOp)
        }
    }
