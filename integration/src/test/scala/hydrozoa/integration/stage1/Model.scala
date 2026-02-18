package hydrozoa.integration.stage1

import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.node.operation.liquidation.NodeOperationLiquidationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.integration.stage1.Error.UnexpectedState
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.consensus.DepositRequest
import hydrozoa.multisig.ledger.VirtualLedgerM
import hydrozoa.multisig.ledger.block.{BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.event.LedgerEventId.ValidityFlag
import hydrozoa.multisig.ledger.event.LedgerEventNumber.increment
import hydrozoa.multisig.ledger.event.{LedgerEvent, LedgerEventId, LedgerEventNumber}
import hydrozoa.multisig.ledger.virtual.HydrozoaTransactionMutator
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.kzgCommitment
import hydrozoa.multisig.ledger.virtual.tx.L2Tx
import monocle.syntax.all.focus
import scalus.cardano.ledger.{Transaction, TransactionInput, Utxos}
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
    nextLedgerEventNumber: LedgerEventNumber,
    // Block producing cycle
    currentTime: CurrentTime,
    blockCycle: BlockCycle,
    currentBlockEvents: List[(LedgerEvent, L2Tx | DepositRequest, ValidityFlag)] = List.empty,

    // This is put here to avoid tossing over Done/Ready/InProgress
    // NB: for block zero it's more initializationExpirationTime
    competingFallbackStartTime: QuantizedInstant,

    // L2 state
    activeUtxos: Utxos,

    // L1 state - the only peer's utxos
    peerL1Utxos: Utxos
) {
    override def toString: String = "<model state (hidden)>"

    def nextLedgerEventId: LedgerEventId =
        LedgerEventId(peerNum = ownTestPeer.peerNum, eventNum = nextLedgerEventNumber)

    /** To save time and keep things simple we exploit the fact that all txs that may mutate the L1
      * state of the peer's utxo are continuing - they spend and pays back at least one utxo that
      * belongs to the peer. So we can always calculate peer's addresses using the preexisting
      * state.
      */
    def applyContinuingL1Tx(l1Tx: Transaction): ModelState = {
        val peerAddresses = this.peerL1Utxos.map(_._2.address).toSet
        val survivedUtxo = this.peerL1Utxos -- l1Tx.body.value.inputs.toSet
        val newUtxos = survivedUtxo ++ l1Tx.body.value.outputs.toList
            .map(_.value)
            .zipWithIndex
            .filter((output, _) => peerAddresses.contains(output.address))
            .map((output, ix) => TransactionInput(l1Tx.id, ix) -> output)
        this.copy(peerL1Utxos = newUtxos)
    }
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

given ModelCommand[DelayCommand, Unit, ModelState] with {

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

given ModelCommand[StartBlockCommand, Unit, ModelState] with {

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

given ModelCommand[L2TxCommand, Unit, ModelState] with {

    override def runState(cmd: L2TxCommand, state: ModelState): (Unit, ModelState) =

        val l2Tx: L2Tx = L2Tx
            .parse(cmd.event.tx)
            .fold(err => throw RuntimeException(s"Failed to parse L2Tx: $err"), identity)

        val ret = HydrozoaTransactionMutator.transit(
          config = state.headConfig,
          time = state.currentTime.instant,
          state = VirtualLedgerM.State(state.activeUtxos),
          l2Tx = l2Tx
        )

        val newState = ret match {
            case Left(err) =>
                logger.debug(s"invalid L2 tx ${cmd.event.eventId}: ${err}")
                state
                    .focus(_.currentBlockEvents)
                    .modify(_ :+ (cmd.event, l2Tx, ValidityFlag.Invalid))
            case Right(mutatorState) =>
                state
                    .focus(_.currentBlockEvents)
                    .modify(_ :+ (cmd.event, l2Tx, ValidityFlag.Valid))
                    .focus(_.activeUtxos)
                    .replace(mutatorState.activeUtxos)
        }

        () -> newState
            .focus(_.nextLedgerEventNumber)
            .modify(_.increment)
}

given ModelCommand[CompleteBlockCommand, BlockBrief, ModelState] with {

    private val logger: org.slf4j.Logger =
        org.slf4j.LoggerFactory.getLogger(getClass)

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
        currentBlockEvents: List[(LedgerEvent, L2Tx | DepositRequest, ValidityFlag)],
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
            events = currentBlockEvents.map((le, _, flag) => le.eventId -> flag),
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
            events = currentBlockEvents.map((le, _, flag) => le.eventId -> flag),
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
            events = currentBlockEvents.map((le, _, flag) => le.eventId -> flag),
            depositsRefunded = List.empty
          )
        )

        if isFinal then finalBlock
        else if txTiming.blockCanStayMinor(creationTime, competingFallbackStartTime)
        then {
            val hasWithdrawals = currentBlockEvents.exists(_._2 match {
                case e: L2Tx => e.l1utxos.nonEmpty
                case _       => false
            })
            val hasDepositsAbsorbed: Boolean = false

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

given ModelCommand[RegisterDepositCommand, Unit, ModelState] with {
    override def runState(
        cmd: RegisterDepositCommand,
        state: ModelState
    ): (Unit, ModelState) = () -> state
}

enum Error extends Throwable:
    case UnexpectedState(msg: String)

    override def getMessage: String = this match {
        case Error.UnexpectedState(msg) => s"Unexpected state while stepping the model: $msg"
    }
