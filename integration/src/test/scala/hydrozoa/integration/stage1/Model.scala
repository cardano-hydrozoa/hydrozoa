package hydrozoa.integration.stage1

import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.node.operation.liquidation.NodeOperationLiquidationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.integration.stage1.Commands.*
import hydrozoa.integration.stage1.Model.Error.UnexpectedState
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.given_Ordering_QuantizedInstant.mkOrderingOps
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.ledger.JointLedger.mkGenesisId
import hydrozoa.multisig.ledger.VirtualLedgerM
import hydrozoa.multisig.ledger.block.BlockBrief.{Final, Major, Minor}
import hydrozoa.multisig.ledger.block.{BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.dapp.txseq.DepositRefundTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.event.LedgerEventId.ValidityFlag
import hydrozoa.multisig.ledger.event.LedgerEventId.ValidityFlag.Valid
import hydrozoa.multisig.ledger.event.LedgerEventNumber.increment
import hydrozoa.multisig.ledger.event.{LedgerEvent, LedgerEventId, LedgerEventNumber}
import hydrozoa.multisig.ledger.virtual.HydrozoaTransactionMutator
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.kzgCommitment
import hydrozoa.multisig.ledger.virtual.tx.{GenesisObligation, L2Genesis, L2Tx}
import monocle.Lens
import monocle.syntax.all.focus
import org.scalacheck.commands.ModelCommand
import scala.collection.immutable.Queue
import scala.util.chaining.*
import scalus.cardano.ledger.{AssetName, Transaction, TransactionHash, TransactionInput, Utxos}
import test.TestPeer

object Model:
    private val logger = Logging.logger("Stage1.Model")

    // ===================================
    // Model state
    // ===================================

    /** This should be immutable. Only contains what's needed for model operations and SUT
      * construction.
      */
    case class State(
        // Read-only: minimal configuration needed for model and SUT
        // TODO: I wanted TestPeer not to leave the config generation part, check whether it's possible
        //   One place we use it - signing deposit tx during the generation
        ownTestPeer: TestPeer,
        headConfig: HeadConfig,
        operationalMultisigConfig: NodeOperationMultisigConfig,
        operationalLiquidationConfig: NodeOperationLiquidationConfig,

        // "Mutable" part
        nextLedgerEventNumber: LedgerEventNumber,
        currentTime: CurrentTime,
        // Block producing cycle

        blockCycle: BlockCycle,

        // This is put here to avoid tossing over Done/Ready/InProgress
        // NB: for block zero it's more initializationExpirationTime
        competingFallbackStartTime: QuantizedInstant,

        // L2 state
        activeUtxos: Utxos,

        // L1 state - the only peer's utxos
        peerUtxosL1: Utxos,

        // Deposits

        // The queue of deposits that may be submitted if timing is lucky, or discarded as expired.
        // At all times, all deposits in the list are unrelated in terms of funding utxo.
        depositEnqueued: List[RegisterDepositCommand],
        depositsRegistered: List[LedgerEventId],
        // Utxos used in the deposit enqueued as funding utxos.
        // We need this not to generate deposits that use the same utxos for funding many times.
        utxoLocked: Set[TransactionInput],

        // Signed deposit transactions
        depositSigned: Map[TransactionHash, Transaction],

        // Deposits, that have been submitted, so they are expected to appear in the
        // very first block that satisfies their absorption window.
        // Deposits along with their maturity start time
        depositSubmitted: List[LedgerEventId],
        depositRejected: List[LedgerEventId],
    ) {
        override def toString: String = "<model state (hidden)>"

        def nextLedgerEventId: LedgerEventId =
            LedgerEventId(peerNum = ownTestPeer.peerNum, eventNum = nextLedgerEventNumber)

        /** To save time and keep things simple we exploit the fact that all txs that may mutate the
          * L1 state of the peer's utxo are continuing - they spend and pays back at least one utxo
          * that belongs to the peer. So we can always calculate peer's addresses using the
          * preexisting state.
          */
        def applyContinuingL1Tx(l1Tx: Transaction): State = {
            // TODO: this is a bit unwieldy
            val peerAddresses = this.peerUtxosL1.map(_._2.address).toSet
                + ownTestPeer.address(headConfig.network)
            val survivedUtxo = this.peerUtxosL1 -- l1Tx.body.value.inputs.toSet
            val newUtxos = survivedUtxo ++ l1Tx.body.value.outputs.toList
                .map(_.value)
                .zipWithIndex
                .filter((output, _) => peerAddresses.contains(output.address))
                .map((output, ix) => TransactionInput(l1Tx.id, ix) -> output)
            this.copy(peerUtxosL1 = newUtxos)
        }

        def depositForSubmission: List[(LedgerEventId, QuantizedInstant)] =
            this.depositEnqueued
                .map(cmd =>
                    cmd.registerDeposit.eventId -> cmd.depositRefundTxSeq.depositTx.validityEnd
                )
                .filter(e => depositsRegistered.contains(e._1))
                .filterNot(e => depositSubmitted.contains(e._1) || depositRejected.contains(e._1))
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
            blockStartTime: QuantizedInstant,
            // We use previous version here since the current version is not defined
            // until the block will be ended (in the future).
            prevVersion: BlockVersion.Full,
            events: List[
              (
                  // Raw ledger event
                  LedgerEvent,
                  // Parsed counterpart
                  L2Tx | DepositUtxo,
                  // Validity flag
                  ValidityFlag
              )
            ] = List.empty,
        )

        /** The final block is Done */
        case HeadFinalized

    // Shared lenses for accessing BlockCycle.InProgress and its events
    private val blockCycleLens: Lens[State, BlockCycle.InProgress] =
        Lens[State, BlockCycle.InProgress](
          get = _.blockCycle.asInstanceOf[BlockCycle.InProgress]
        )(
          replace = bc => s => s.copy(blockCycle = bc)
        )

    private val eventsLens
        : Lens[BlockCycle.InProgress, List[(LedgerEvent, L2Tx | DepositUtxo, ValidityFlag)]] =
        Lens[BlockCycle.InProgress, List[(LedgerEvent, L2Tx | DepositUtxo, ValidityFlag)]](
          get = _.events
        )(
          replace = events => bc => bc.copy(events = events)
        )

    // ===================================
    // ModelCommand instances
    // ===================================

    given ModelCommand[DelayCommand, Unit, State] with {

        override def runState(cmd: DelayCommand, state: State): (Unit, State) = {
            logger.debug(s"MODEL>> DelayCommand: ${cmd.delaySpec}")
            val newBlock = state.blockCycle match {
                case BlockCycle.Done(blockNumber, version) =>
                    logger.trace(s"Transitioning Done -> Ready for block ${blockNumber}")
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

    given ModelCommand[StartBlockCommand, Unit, State] with {

        override def runState(cmd: StartBlockCommand, state: State): (Unit, State) = {
            logger.debug(s"MODEL>> StartBlockCommand for block number: ${cmd.blockNumber}")

            state.currentTime match {
                case CurrentTime.BeforeHappyPathExpiration(_) =>
                    val newBlock = state.blockCycle match {
                        case BlockCycle.Ready(prevBlockNumber, prevVersion)
                            if prevBlockNumber.increment == cmd.blockNumber =>
                            BlockCycle.InProgress(
                              blockNumber = cmd.blockNumber,
                              blockStartTime = cmd.creationTime,
                              prevVersion = prevVersion
                            )
                        case _ =>
                            throw UnexpectedState("StartBlockCommand requires BlockCycle.Ready")
                    }
                    () -> state.copy(blockCycle = newBlock)
                case _ =>
                    throw Error.UnexpectedState(
                      "StartBlockCommand requires CurrentTime.BeforeHappyPathExpiration"
                    )
            }
        }
    }

    given ModelCommand[L2TxCommand, Unit, State] with {

        override def runState(cmd: L2TxCommand, state: State): (Unit, State) =

            logger.debug(s"MODEL>> L2TxCommand for event ID: ${cmd.event.eventId}")

            val BlockCycle.InProgress(_, _, _, currentEvents) = state.blockCycle: @unchecked
            logger.trace(s"INPUT state.blockCycle event IDs: ${currentEvents.map(_._1.eventId)}")

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
                    blockCycleLens
                        .andThen(eventsLens)
                        .modify(_ :+ (cmd.event, l2Tx, ValidityFlag.Invalid))(state)
                case Right(mutatorState) =>
                    state
                        .pipe(
                          blockCycleLens
                              .andThen(eventsLens)
                              .modify(_ :+ (cmd.event, l2Tx, ValidityFlag.Valid))
                        )
                        .focus(_.activeUtxos)
                        .replace(mutatorState.activeUtxos)
            }

            val finalState = newState
                .focus(_.nextLedgerEventNumber)
                .modify(_.increment)

            val BlockCycle.InProgress(_, _, _, finalEvents) = finalState.blockCycle: @unchecked
            logger.trace(
              s"OUTPUT finalState.blockCycle event IDs: ${finalEvents.map(_._1.eventId)}"
            )

            () -> finalState
    }

    given ModelCommand[CompleteBlockCommand, BlockBrief, State] with {

        override def runState(
            cmd: CompleteBlockCommand,
            state: State
        ): (BlockBrief, State) = {
            logger.debug(s"MODEL>> CompleteBlockCommand for block number: ${cmd.blockNumber}")
            state.blockCycle match {
                case BlockCycle.InProgress(_, creationTime, prevVersion, events) =>
                    logger.trace(
                      s"Completing block with ${events.length} events: ${events.map(_._1.eventId)}"
                    )
                    val (blockBrief, newActiveUtxos, withdrawnUtxos) = mkBlockBrief(
                      cmd.blockNumber,
                      events,
                      state.competingFallbackStartTime,
                      state.headConfig.txTiming,
                      creationTime,
                      prevVersion,
                      cmd.isFinal,
                      state.activeUtxos,
                      state.depositEnqueued,
                      state.depositsRegistered,
                      state.depositSubmitted,
                      state.headConfig.headTokenNames.treasuryTokenName
                    )

                    val newCompetingFallbackStartTime =
                        if blockBrief.isInstanceOf[Major]
                        then state.headConfig.txTiming.newFallbackStartTime(creationTime)
                        else state.competingFallbackStartTime
                    logger.debug(s"newCompetingFallbackStartTime: $newCompetingFallbackStartTime")

                    val newState = state.copy(
                      blockCycle =
                          if cmd.isFinal then BlockCycle.HeadFinalized
                          else BlockCycle.Done(cmd.blockNumber, blockBrief.blockVersion),
                      competingFallbackStartTime = newCompetingFallbackStartTime,
                      // Remove handled deposits and add registered deposits
                      depositEnqueued = state.depositEnqueued.filterNot(cmd =>
                          (blockBrief.depositsAbsorbed ++ blockBrief.depositsRefunded).contains(
                            cmd._1.eventId
                          )
                      ),
                      depositsRegistered = state.depositsRegistered
                          ++ blockBrief.events
                              .filter(_._2 == Valid)
                              .map(_._1)
                              .filter(e =>
                                  state.depositEnqueued.map(_.registerDeposit.eventId).contains(e)
                              ),
                      activeUtxos = newActiveUtxos,
                      peerUtxosL1 = state.peerUtxosL1 ++ withdrawnUtxos
                    )
                    logger.trace(
                      s"block ${cmd.blockNumber}, newState.depositEnqueued=${newState.depositEnqueued}, " +
                          s"newState.depositsRegistered=${newState.depositsRegistered}"
                    )
                    blockBrief -> newState
                case _ =>
                    throw UnexpectedState("CompleteBlockCommand requires BlockCycle.InProgress")
            }
        }

        private def mkBlockBrief(
            blockNumber: BlockNumber,
            events: List[(LedgerEvent, L2Tx | DepositUtxo, ValidityFlag)],
            competingFallbackStartTime: QuantizedInstant,
            txTiming: TxTiming,
            blockStartTime: QuantizedInstant,
            prevVersion: BlockVersion.Full,
            isFinal: Boolean,
            activeUtxos: Utxos,
            depositEnqueued: List[RegisterDepositCommand],
            depositRegistered: List[LedgerEventId],
            depositsSubmitted: List[LedgerEventId],
            treasuryTokenName: AssetName
        ): (BlockBrief, Utxos, Utxos) = {

            logger.trace(s"mkBlockBrief: blockNumber: $blockNumber")
            logger.trace(s"mkBlockBrief: blockStartTime: $blockStartTime")
            val settlementValidityEnd =
                txTiming.newSettlementEndTime(competingFallbackStartTime)
            logger.trace(s"settlementValidityEnd=$settlementValidityEnd")

            val events_ = events.map((le, _, flag) => le.eventId -> flag)

            val depositsToCheck =
                depositEnqueued.filter(d => depositRegistered.contains(d.registerDeposit.eventId))

            val depositsAbsorbed = depositsToCheck
                .filter(cmd => {
                    val submissionDeadline = cmd.depositRefundTxSeq.depositTx.validityEnd
                    val depositAbsorptionStart =
                        txTiming.depositAbsorptionStartTime(submissionDeadline)
                    val depositAbsorptionEnd = txTiming.depositAbsorptionEndTime(submissionDeadline)
                    val depositUtxoFound = depositsSubmitted.contains(cmd.registerDeposit.eventId)

                    logger.trace(
                      s"MODEL deposit eligibility check: ${cmd.registerDeposit.eventId},\n" +
                          s"depositAbsorptionStart=$depositAbsorptionStart, " +
                          s"depositAbsorptionEnd=$depositAbsorptionEnd, " +
                          s"utxo is found: $depositUtxoFound"
                    )

                    depositAbsorptionStart <= blockStartTime
                    && depositAbsorptionEnd >= settlementValidityEnd
                    && depositUtxoFound
                })
                .map(_._1.eventId)

            logger.trace(s"depositsAbsorbed: $depositsAbsorbed")

            val depositsRefunded =
                (if isFinal
                 then depositsToCheck // all known deposits should be refunded
                 else
                     depositsToCheck.filter(cmd =>
                         val submissionDeadline = cmd.depositRefundTxSeq.depositTx.validityEnd
                         val depositAbsorptionStart =
                             txTiming.depositAbsorptionStartTime(submissionDeadline)
                         val settlementValidityEnd =
                             txTiming.newSettlementEndTime(competingFallbackStartTime)
                         val depositAbsorptionEnd =
                             txTiming.depositAbsorptionEndTime(submissionDeadline)
                         val depositUtxoFound =
                             depositsSubmitted.contains(cmd.registerDeposit.eventId)

                         logger.trace(
                           s"MODEL deposit rejection check: ${cmd.registerDeposit.eventId},\n" +
                               s"depositAbsorptionEnd=$depositAbsorptionEnd, " +
                               s"settlementValidityEnd=$settlementValidityEnd"
                         )

                         settlementValidityEnd > depositAbsorptionEnd ||
                         (depositAbsorptionStart < blockStartTime && !depositUtxoFound)
                     )
                ).map(_._1.eventId)

            logger.trace(s"depositsRefunded: $depositsRefunded")

            val genesisObligations: List[GenesisObligation] =
                depositsAbsorbed
                    .map(eventId =>
                        depositEnqueued
                            .find(_.registerDeposit.eventId == eventId)
                            .getOrElse(
                              throw RuntimeException(s"deposit not found: $eventId")
                            )
                    )
                    .flatMap(_.depositRefundTxSeq.depositTx.depositProduced.virtualOutputs.toList)

            val genesisUtxos: Option[Utxos] = for {
                obligations <-
                    if genesisObligations.nonEmpty
                    then Some(genesisObligations)
                    else None
                genesisId = mkGenesisId(treasuryTokenName, prevVersion.incrementMajor.major)
                l2Genesis = L2Genesis(Queue.from(obligations), genesisId)
            } yield l2Genesis.asUtxos

            val newActiveUtxos = activeUtxos ++ genesisUtxos.getOrElse(Utxos.empty)

            // Extract all L1-bound UTXOs from L2Tx events
            // TODO: this is not correct, mention in the comments
            val withdrawnUtxos: Utxos = Utxos.empty
            // val withdrawnUtxos: Utxos = events.flatMap {
            //    case (_, l2tx: L2Tx, _) => l2tx.l1utxos
            //    case _                  => List.empty
            // }.toMap

            lazy val majorBlock = Major(
              header = BlockHeader.Major(
                blockNum = blockNumber,
                blockVersion = prevVersion.incrementMajor,
                startTime = blockStartTime,
                kzgCommitment = newActiveUtxos.kzgCommitment
              ),
              body = BlockBody.Major(
                events = events_,
                depositsAbsorbed = depositsAbsorbed,
                depositsRefunded = depositsRefunded
              )
            )

            lazy val minorBlock = Minor(
              header = BlockHeader.Minor(
                blockNum = blockNumber,
                blockVersion = prevVersion.incrementMinor,
                startTime = blockStartTime,
                kzgCommitment = activeUtxos.kzgCommitment
              ),
              body = BlockBody.Minor(
                events = events.map((le, _, flag) => le.eventId -> flag),
                depositsRefunded = depositsRefunded
              )
            )

            lazy val finalBlock = Final(
              header = BlockHeader.Final(
                blockNum = blockNumber,
                blockVersion = prevVersion.incrementMajor,
                startTime = blockStartTime,
              ),
              body = BlockBody.Final(
                events = events.map((le, _, flag) => le.eventId -> flag),
                depositsRefunded = depositsRefunded
              )
            )

            val brief =
                if isFinal then finalBlock
                else if txTiming.blockCanStayMinor(blockStartTime, competingFallbackStartTime)
                then {
                    val hasWithdrawals = events.exists(_._2 match {
                        case e: L2Tx => e.l1utxos.nonEmpty
                        case _       => false
                    })
                    val hasDepositsAbsorbed: Boolean = depositsAbsorbed.nonEmpty

                    if hasWithdrawals || hasDepositsAbsorbed
                    then majorBlock
                    else minorBlock
                } else majorBlock

            logger.trace(s"block brief: $brief")

            (brief, newActiveUtxos, withdrawnUtxos)
        }

        override def preCondition(cmd: CompleteBlockCommand, state: State): Boolean =
            state.blockCycle match {
                case BlockCycle.InProgress(currentBlockNumber, _, _, _) =>
                    cmd.blockNumber == currentBlockNumber
                case _ => false
            }
    }

    given ModelCommand[RegisterDepositCommand, Unit, State] with {
        override def runState(
            cmd: RegisterDepositCommand,
            state: State
        ): (Unit, State) = {

            import cmd.registerDeposit as req
            import state.headConfig as config

            logger.debug(
              s"MODEL>> RegisterDepositCommand for event ID: ${cmd.registerDeposit.eventId}"
            )

            val BlockCycle.InProgress(_, blockStartTime, _, currentEvents) =
                state.blockCycle: @unchecked
            logger.trace(s"INPUT state.blockCycle event IDs: ${currentEvents.map(_._1.eventId)}")

            val seq =
                DepositRefundTxSeq
                    .Parse(config)(
                      depositTxBytes = req.depositTxBytes,
                      refundTxBytes = req.refundTxBytes,
                      virtualOutputsBytes = req.virtualOutputsBytes
                    )
                    .result
                    .fold(e => throw RuntimeException(e), identity)

            // For now, all deposits request should be valid by construction
            require(blockStartTime < seq.depositTx.validityEnd)
            require(
              blockStartTime < config.txTiming.depositAbsorptionEndTime(seq.depositTx.validityEnd)
            )

            logger.trace(s"deposit txHash=${seq.depositTx.tx.id}")

            val depositUtxo = seq.depositTx.depositProduced

            val finalState = state
                .pipe(
                  blockCycleLens
                      .andThen(eventsLens)
                      .modify(_ :+ (cmd.registerDeposit, depositUtxo, ValidityFlag.Valid))
                )
                .focus(_.depositEnqueued)
                .modify(_ :+ cmd)
                .focus(_.utxoLocked)
                .modify(_ ++ seq.depositTx.tx.body.value.inputs.toSeq)
                .focus(_.depositSigned)
                .modify(_ + (cmd.depositTxBytesSigned.id -> cmd.depositTxBytesSigned))
                .focus(_.nextLedgerEventNumber)
                .modify(_.increment)

            val BlockCycle.InProgress(_, _, _, finalEvents) = finalState.blockCycle: @unchecked

            logger.trace(
              s"OUTPUT finalState.blockCycle event IDs: ${finalEvents.map(_._1.eventId)}"
            )
            logger.trace(
              s"OUTPUT finalState.depositEnqueued IDs: ${finalState.depositEnqueued
                      .map(e => e._1.eventId -> e._2.depositTx.validityEnd)}"
            )

            () -> finalState
        }
    }

    given ModelCommand[SubmitDepositsCommand, Unit, State] with {
        override def runState(
            cmd: SubmitDepositsCommand,
            state: State
        ): (Unit, State) = {
            logger.debug(
              s"MODEL>> SubmitDepositCommand, for submission: ${cmd.depositsForSubmission.size}, for rejection: ${cmd.depositsForRejection.size}"
            )

            val allDeposits = cmd.depositsForSubmission.map(e => true -> e._1)
                ++ cmd.depositsForRejection.map(e => false -> e)

            // Attach corresponding register deposit commands
            val depositsAug = allDeposits.map((flag, eventId) =>
                val registerDepositCmd = state.depositEnqueued
                    .find(_.registerDeposit.eventId == eventId)
                    .getOrElse(
                      throw RuntimeException(
                        s"Deposit with event ID $eventId not found in enqueued"
                      )
                    )
                (flag, eventId, registerDepositCmd)
            )

            // Apply each deposit transaction to peerUtxosL1, checking TTL
            // Also collect deposit UTXO IDs (output 0 of deposit txs)
            val newState =
                depositsAug.foldLeft(state) { case (acc, (flag, eventId, registerDepositCmd)) =>
                    val signedTx = registerDepositCmd.depositTxBytesSigned
                    val fundingUtxos = signedTx.body.value.inputs.toSet

                    if flag then
                        // "submit" deposit tx
                        logger.trace(s"Stepping model: submit deposit tx for event ID: $eventId")
                        acc.copy(
                          depositSubmitted = acc.depositSubmitted :+ eventId
                        ).applyContinuingL1Tx(signedTx)
                    else
                        // Unlock funding utxos
                        logger.info(
                          s"Deposit $eventId is expired, unlocking funding utxos"
                        )
                        acc.copy(
                          utxoLocked = acc.utxoLocked -- fundingUtxos,
                          depositRejected = acc.depositRejected :+ eventId
                        )
                }

            () -> newState
        }
    }

    enum Error extends Throwable:
        case UnexpectedState(msg: String)

        override def getMessage: String = this match {
            case Error.UnexpectedState(msg) => s"Unexpected state while stepping the model: $msg"
        }

end Model
