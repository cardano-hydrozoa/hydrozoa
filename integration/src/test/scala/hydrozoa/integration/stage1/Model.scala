package hydrozoa.integration.stage1

import cats.data.NonEmptyList
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.integration.stage1.Error.UnexpectedState
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.block.{BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.event.LedgerEvent
import scalus.cardano.ledger.{CardanoInfo, Transaction, TransactionInput, TransactionOutput, Utxo}
import test.TestPeer

// ===================================
// Model state
// ===================================

/** This should be immutable. Only contains what's needed for model operations and SUT construction.
  */
case class ModelState(
    // Read-only: minimal configuration needed for model and SUT
    // We don't want to have the whole Head/Peer config here
    ownTestPeer: TestPeer,
    txTiming: TxTiming,
    equityShares: Void, // EquityShares,
    spentUtxos: NonEmptyList[Utxo],
    initTxSigned: Transaction,
    fallbackTxSigned: Transaction,
    // Needed for command generators
    cardanoInfo: CardanoInfo,

    // Block producing cycle
    currentTime: CurrentTime,
    blockCycle: BlockCycle,
    currentBlockEvents: List[LedgerEvent] = List.empty,

    // This is put here to avoid tossing over Done/Ready/InProgress
    // NB: for block zero it's more initializationExpirationTime
    competingFallbackStartTime: QuantizedInstant,

    // L2 state
    activeUtxos: Map[TransactionInput, TransactionOutput],
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
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockBody, BlockHeader}
import hydrozoa.multisig.ledger.event.LedgerEventId.ValidityFlag
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

implicit object LedgerEventCommandModel extends ModelCommand[LedgerEventCommand, Unit, ModelState] {

    override def runState(cmd: LedgerEventCommand, state: ModelState): (Unit, ModelState) =
        () -> state.copy(currentBlockEvents = state.currentBlockEvents :+ cmd.event)
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
                  state.txTiming,
                  creationTime,
                  prevVersion,
                  cmd.isFinal
                )
                val newCompetingFallbackStartTime =
                    if result.isInstanceOf[Major]
                    then state.txTiming.newFallbackStartTime(creationTime)
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
        currentBlockEvents: List[LedgerEvent],
        competingFallbackStartTime: QuantizedInstant,
        txTiming: TxTiming,
        creationTime: QuantizedInstant,
        prevVersion: BlockVersion.Full,
        isFinal: Boolean
    ): BlockBrief = {

        if isFinal then
            Final(
              header = BlockHeader.Final(
                blockNum = blockNumber,
                blockVersion = prevVersion.incrementMajor,
                startTime = creationTime,
              ),
              body = BlockBody.Final(
                events = currentBlockEvents.map(_.eventId -> ValidityFlag.Invalid),
                depositsRefunded = List.empty
              )
            )
        else if txTiming.blockCanStayMinor(creationTime, competingFallbackStartTime)
        then
            Minor(
              header = BlockHeader.Minor(
                blockNum = blockNumber,
                blockVersion = prevVersion.incrementMinor,
                startTime = creationTime,
                kzgCommitment = KzgCommitment.empty
              ),
              body = BlockBody.Minor(
                events = currentBlockEvents.map(_.eventId -> ValidityFlag.Invalid),
                depositsRefunded = List.empty
              )
            )
        else
            Major(
              header = BlockHeader.Major(
                blockNum = blockNumber,
                blockVersion = prevVersion.incrementMajor,
                startTime = creationTime,
                kzgCommitment = KzgCommitment.empty
              ),
              body = BlockBody.Major(
                events = currentBlockEvents.map(_.eventId -> ValidityFlag.Invalid),
                depositsAbsorbed = List.empty,
                depositsRefunded = List.empty
              )
            )
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
