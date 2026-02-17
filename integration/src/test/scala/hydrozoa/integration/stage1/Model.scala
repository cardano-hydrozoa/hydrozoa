package hydrozoa.integration.stage1

import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.node.operation.liquidation.NodeOperationLiquidationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.integration.stage1.Error.UnexpectedState
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.ledger.VirtualLedgerM
import hydrozoa.multisig.ledger.block.{BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.event.LedgerEvent
import hydrozoa.multisig.ledger.event.LedgerEventId.ValidityFlag
import hydrozoa.multisig.ledger.virtual.HydrozoaTransactionMutator
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.kzgCommitment
import scalus.cardano.ledger.Utxos
import test.TestPeer

val logger = Logging.logger("Suite1.Model")

// ===================================
// Model state
// ===================================

/** This should be immutable. Only contains what's needed for model operations and SUT construction.
  */
case class ModelState(
    // Read-only: minimal configuration needed for model and SUT
    // TODO: I wanted TestPeer not to leave the config generation part, check whether it's possible
    ownTestPeer: TestPeer,
    headConfig: HeadConfig,
    operationalMultisigConfig: NodeOperationMultisigConfig,
    operationalLiquidationConfig: NodeOperationLiquidationConfig,

    // "Mutable" part
    // Block producing cycle
    currentTime: CurrentTime,
    blockCycle: BlockCycle,
    currentBlockEvents: List[(LedgerEvent, ValidityFlag)] = List.empty,

    // This is put here to avoid tossing over Done/Ready/InProgress
    // NB: for block zero it's more initializationExpirationTime
    competingFallbackStartTime: QuantizedInstant,

    // L2 state
    activeUtxos: Utxos,
) {
    override def toString: String = "<model state (hidden)>"
}

enum CurrentTime(qi: QuantizedInstant):
    case BeforeHappyPathExpiration(qi: QuantizedInstant) extends CurrentTime(qi)

    case InSilencePeriod(qi: QuantizedInstant) extends CurrentTime(qi)

    case AfterCompetingFallbackStartTime(qi: QuantizedInstant) extends CurrentTime(qi)

    def instant: QuantizedInstant = qi

enum BlockCycle:

    /** Block is done, delay is ahead.
      */
    case Done(
        blockNumber: BlockNumber,
        version: BlockVersion.Full
    )

    /** Delay is over, ready for a new block. */
    case Ready(
        blockNumber: BlockNumber,
        // We use previous version here since the current version is not defined
        // until the block will be ended (in the future).
        prevVersion: BlockVersion.Full
    )

    /** Block is under construction. */
    case InProgress(
        blockNumber: BlockNumber,
        creationTime: QuantizedInstant,
        // We use previous version here since the current version is not defined
        // until the block will be ended (in the future).
        prevVersion: BlockVersion.Full
    )

    /** The final block is Done */
    case HeadFinalized

// ===================================
// ModelCommand instances
// ===================================

import hydrozoa.multisig.ledger.block.BlockBrief.{Final, Major, Minor}
import hydrozoa.multisig.ledger.block.{BlockBody, BlockBrief, BlockHeader}
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import org.scalacheck.commands.ModelCommand

implicit object DelayCommandModel extends ModelCommand[DelayCommand, Unit, ModelState] {

    override def runState(cmd: DelayCommand, state: ModelState): (Unit, ModelState) = {
        val newBlock = state.blockCycle match {
            case BlockCycle.Done(blockNumber, version) =>
                BlockCycle.Ready(blockNumber = blockNumber, prevVersion = version)
            case _ => throw Error.UnexpectedState("DelayCommand requires BlockCycle.Done")
        }
        val instant = state.currentTime.instant + cmd.delaySpec.duration
        () -> state.copy(
          blockCycle = newBlock,
          currentTime = cmd.delaySpec match {
              case Delay.EndsBeforeHappyPathExpires(_) =>
                  CurrentTime.BeforeHappyPathExpiration(instant)
              case Delay.EndsInTheSilencePeriod(_) => CurrentTime.InSilencePeriod(instant)
              case Delay.EndsAfterHappyPathExpires(_) =>
                  CurrentTime.AfterCompetingFallbackStartTime(instant)
          }
        )
    }

    override def delay(cmd: DelayCommand): scala.concurrent.duration.FiniteDuration =
        cmd.delaySpec.duration.finiteDuration

}

implicit object StartBlockCommandModel extends ModelCommand[StartBlockCommand, Unit, ModelState] {

    override def runState(cmd: StartBlockCommand, state: ModelState): (Unit, ModelState) = {
        state.currentTime match {
            case CurrentTime.BeforeHappyPathExpiration(_) =>
                val newBlock = state.blockCycle match {
                    case BlockCycle.Ready(prevBlockNumber, prevVersion)
                        if prevBlockNumber.increment == cmd.blockNumber =>
                        BlockCycle.InProgress(
                          blockNumber = cmd.blockNumber,
                          creationTime = cmd.creationTime,
                          prevVersion = prevVersion
                        )
                    case _ => throw UnexpectedState("StartBlockCommand requires BlockCycle.Ready")
                }
                () -> state.copy(blockCycle = newBlock)
            case _ =>
                throw Error.UnexpectedState(
                  "StartBlockCommand requires CurrentTime.BeforeHappyPathExpiration"
                )
        }
    }
}

implicit object LedgerEventCommandModel extends ModelCommand[L2TxCommand, Unit, ModelState] {

    override def runState(cmd: L2TxCommand, state: ModelState): (Unit, ModelState) =

        val l2TransactionEvent = ??? // L2Tx.apply(cmd.event.tx)
        val ret = HydrozoaTransactionMutator.transit(
          config = state.headConfig,
          time = state.currentTime.instant,
          state = VirtualLedgerM.State(state.activeUtxos),
          l2Tx = l2TransactionEvent
        )
        ret match {
            case Left(err) =>
                logger.debug(s"invalid L2 tx ${cmd.event.eventId}: ${err}")
                () -> state.copy(
                  currentBlockEvents =
                      state.currentBlockEvents :+ (cmd.event -> ValidityFlag.Invalid),
                )
            case Right(mutatorState) =>
                () -> state.copy(
                  currentBlockEvents =
                      state.currentBlockEvents :+ (cmd.event -> ValidityFlag.Valid),
                  activeUtxos = mutatorState.activeUtxos
                )
        }
}

implicit object CompleteBlockCommandModel
    extends ModelCommand[CompleteBlockCommand, BlockBrief, ModelState] {

    private val logger: org.slf4j.Logger =
        org.slf4j.LoggerFactory.getLogger(CompleteBlockCommandModel.getClass)

    override def runState(
        cmd: CompleteBlockCommand,
        state: ModelState
    ): (BlockBrief, ModelState) = {
        state.blockCycle match {
            case BlockCycle.InProgress(_, creationTime, prevVersion) =>
                val result = mkBlockBrief(
                  cmd.blockNumber,
                  state.currentBlockEvents,
                  state.competingFallbackStartTime,
                  state.headConfig.txTiming,
                  creationTime,
                  prevVersion,
                  cmd.isFinal,
                  state.activeUtxos
                )
                val newCompetingFallbackStartTime =
                    if result.isInstanceOf[Major]
                    then state.headConfig.txTiming.newFallbackStartTime(creationTime)
                    else state.competingFallbackStartTime
                logger.debug(s"newCompetingFallbackStartTime: $newCompetingFallbackStartTime")

                val newState = state.copy(
                  blockCycle =
                      if cmd.isFinal then BlockCycle.HeadFinalized
                      else BlockCycle.Done(cmd.blockNumber, result.blockVersion),
                  currentBlockEvents = List.empty,
                  competingFallbackStartTime = newCompetingFallbackStartTime
                )
                result -> newState
            case _ => throw UnexpectedState("CompleteBlockCommand requires BlockCycle.InProgress")
        }
    }

    private def mkBlockBrief(
        blockNumber: BlockNumber,
        currentBlockEvents: List[(LedgerEvent, ValidityFlag)],
        competingFallbackStartTime: QuantizedInstant,
        txTiming: TxTiming,
        creationTime: QuantizedInstant,
        prevVersion: BlockVersion.Full,
        isFinal: Boolean,
        activeUtxos: Utxos
    ): BlockBrief = {

        lazy val majorBlock = Major(
          header = BlockHeader.Major(
            blockNum = blockNumber,
            blockVersion = prevVersion.incrementMajor,
            startTime = creationTime,
            kzgCommitment = activeUtxos.kzgCommitment
          ),
          body = BlockBody.Major(
            events = currentBlockEvents.map((le, flag) => le.eventId -> flag),
            depositsAbsorbed = List.empty,
            depositsRefunded = List.empty
          )
        )

        lazy val minorBlock = Minor(
          header = BlockHeader.Minor(
            blockNum = blockNumber,
            blockVersion = prevVersion.incrementMinor,
            startTime = creationTime,
            kzgCommitment = activeUtxos.kzgCommitment
          ),
          body = BlockBody.Minor(
            events = currentBlockEvents.map((le, flag) => le.eventId -> flag),
            depositsRefunded = List.empty
          )
        )

        lazy val finalBlock = Final(
          header = BlockHeader.Final(
            blockNum = blockNumber,
            blockVersion = prevVersion.incrementMajor,
            startTime = creationTime,
          ),
          body = BlockBody.Final(
            events = currentBlockEvents.map((le, flag) => le.eventId -> flag),
            depositsRefunded = List.empty
          )
        )

        if isFinal then finalBlock
        else if txTiming.blockCanStayMinor(creationTime, competingFallbackStartTime)
        then {
            val hasWithdrawals = currentBlockEvents.exists(_._1 match {
                case e: LedgerEvent.TxL2Event => ??? // e.outputPartition.l1Utxos.nonEmpty
                case _                        => false
            })
            val hasDepositsAbsorbed: Boolean = ???

            if hasWithdrawals || hasDepositsAbsorbed
            then majorBlock
            else minorBlock
        } else majorBlock
    }

    override def preCondition(cmd: CompleteBlockCommand, state: ModelState): Boolean =
        state.blockCycle match {
            case BlockCycle.InProgress(currentBlockNumber, _, _) =>
                cmd.blockNumber == currentBlockNumber
            case _ => false
        }
}

enum Error extends Throwable:
    case UnexpectedState(msg: String)

    override def getMessage: String = this match {
        case Error.UnexpectedState(msg) => s"Unexpected state while stepping the model: $msg"
    }
