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
import hydrozoa.multisig.ledger.event.LedgerEventNumber.increment
import hydrozoa.multisig.ledger.event.{LedgerEvent, LedgerEventId, LedgerEventNumber}
import hydrozoa.multisig.ledger.virtual.tx.{GenesisObligation, L2Genesis, L2Tx}
import hydrozoa.multisig.ledger.virtual.{EvacuationMap, HydrozoaTransactionMutator}
import monocle.Lens
import monocle.syntax.all.focus
import org.scalacheck.commands.ModelCommand
import scala.collection.immutable.{Queue, TreeMap}
import scala.util.chaining.*
import scalus.cardano.ledger.{AssetName, KeepRaw, Transaction, TransactionHash, TransactionInput, TransactionOutput, Utxos}
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
        evacuationMap: EvacuationMap[TransactionInput],

        // L1 state - the only peer's utxos
        peerUtxosL1: Utxos,

        // Deposits
        // The queue of deposits that may be submitted if timing is lucky, or discarded as expired
        depositEnqueued: List[RegisterDepositCommand],
        // Utxos used in the deposit enqueued as funding utxos.
        // We need this not to generate deposits that use the same utxos for funding many times.
        utxoLocked: List[TransactionInput],
        // Signed deposit transactions
        depositSigned: Map[TransactionHash, Transaction],

        // TODO: these two seem to be a bit redundant?
        // Deposits, that have been submitted, so they are expected to appear in the
        // very first block that satisfies their absorption window.
        // Deposits along with their maturity start time
        depositSubmitted: List[LedgerEventId],
        // Deposit UTXO IDs (output 0 of deposit txs) that have been submitted
        depositUtxoIds: Set[TransactionInput]
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
            val peerAddresses = this.peerUtxosL1.map(_._2.address).toSet
            val survivedUtxo = this.peerUtxosL1 -- l1Tx.body.value.inputs.toSet
            val newUtxos = survivedUtxo ++ l1Tx.body.value.outputs.toList
                .map(_.value)
                .zipWithIndex
                .filter((output, _) => peerAddresses.contains(output.address))
                .map((output, ix) => TransactionInput(l1Tx.id, ix) -> output)
            this.copy(peerUtxosL1 = newUtxos)
        }

        def depositForSubmission: Set[LedgerEventId] =
            this.depositEnqueued.map(_.registerDeposit.eventId).toSet --
                depositSubmitted.toSet
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
            logger.debug("MODEL>> DelayCommand")
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
              state = VirtualLedgerM.State(state.evacuationMap),
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
                        .focus(_.evacuationMap)
                        .replace(mutatorState.evacuationMap)
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
                    val (blockBrief, newActiveUtxos) = mkBlockBrief(
                      cmd.blockNumber,
                      events,
                      state.competingFallbackStartTime,
                      state.headConfig.txTiming,
                      creationTime,
                      prevVersion,
                      cmd.isFinal,
                      state.evacuationMap,
                      state.depositEnqueued,
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
                      // Remove handled deposits
                      depositEnqueued = state.depositEnqueued.filterNot(cmd =>
                          (blockBrief.depositsAbsorbed ++ blockBrief.depositsRefunded).contains(
                            cmd._1.eventId
                          )
                      ),
                      evacuationMap = newActiveUtxos
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
            evacuationMap: EvacuationMap[TransactionInput],
            depositEnqueued: List[RegisterDepositCommand],
            depositSubmitted: List[LedgerEventId],
            treasuryTokenName: AssetName
        ): (BlockBrief, EvacuationMap[TransactionInput]) = {

            logger.trace(s"mkBlockBrief: blockNumber: $blockNumber")
            logger.trace(s"mkBlockBrief: blockStartTime: $blockStartTime")

            val events_ = events.map((le, _, flag) => le.eventId -> flag)

            val depositsAbsorbed = depositEnqueued
                .filter(cmd => {
                    val submissionDeadline = cmd.depositRefundTxSeq.depositTx.validityEnd
                    val settlementValidityEnd =
                        txTiming.newSettlementEndTime(competingFallbackStartTime)
                    val depositAbsorptionStart =
                        txTiming.depositAbsorptionStartTime(submissionDeadline)
                    val depositAbsorptionEnd = txTiming.depositAbsorptionEndTime(submissionDeadline)
                    val depositUtxoFound = depositSubmitted.contains(cmd.registerDeposit.eventId)

                    logger.trace(
                      s"MODEL deposit check - eligible: ${cmd.registerDeposit.eventId},\n" +
                          s"depositAbsorptionStart=$depositAbsorptionStart, " +
                          s"depositAbsorptionEnd=$depositAbsorptionEnd, " +
                          s"settlementValidityEnd=$settlementValidityEnd, " +
                          s"utxo is found: $depositUtxoFound"
                    )

                    depositAbsorptionStart < blockStartTime
                    && depositAbsorptionEnd > settlementValidityEnd
                    && depositUtxoFound
                })
                .map(_._1.eventId)

            logger.trace(s"depositsAbsorbed: $depositsAbsorbed")

            val depositsRefunded =
                (if isFinal
                 then depositEnqueued // all known deposits should be refunded
                 else
                     depositEnqueued.filter(cmd =>
                         val submissionDeadline = cmd.depositRefundTxSeq.depositTx.validityEnd
                         val settlementValidityEnd =
                             txTiming.newSettlementEndTime(competingFallbackStartTime)
                         val depositAbsorptionEnd =
                             txTiming.depositAbsorptionEndTime(submissionDeadline)

                         logger.trace(
                           s"MODEL deposit check - ineligible: ${cmd.registerDeposit.eventId},\n" +
                               s"depositAbsorptionEnd=$depositAbsorptionEnd, " +
                               s"settlementValidityEnd=$settlementValidityEnd"
                         )

                         settlementValidityEnd > depositAbsorptionEnd
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

            val genesisUtxos: Option[TreeMap[TransactionInput, KeepRaw[TransactionOutput]]] = for {
                obligations <-
                    if genesisObligations.nonEmpty
                    then Some(genesisObligations)
                    else None
                genesisId = mkGenesisId(treasuryTokenName, prevVersion.incrementMajor.major)
                l2Genesis = L2Genesis(Queue.from(obligations), genesisId)
            } yield l2Genesis.asUtxos

            val newActiveUtxos = evacuationMap.appended(
              genesisUtxos.getOrElse(TreeMap.empty[TransactionInput, KeepRaw[TransactionOutput]])
            )

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
                kzgCommitment = evacuationMap.kzgCommitment
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

            (brief, newActiveUtxos)
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

    given ModelCommand[SubmitDepositCommand, Unit, State] with {
        override def runState(
            cmd: SubmitDepositCommand,
            state: State
        ): (Unit, State) = {
            logger.debug(
              s"MODEL>> SubmitDepositCommand for ${cmd.deposits.size} deposits"
            )

            // Attach corresponding register deposit commands
            val depositsAug = cmd.deposits.map((eventId, tx) =>
                val registerDepositCmd = state.depositEnqueued
                    .find(_.registerDeposit.eventId == eventId)
                    .getOrElse(
                      throw RuntimeException(
                        s"Deposit with event ID $eventId not found in enqueued"
                      )
                    )
                (eventId, tx, registerDepositCmd)
            )

            // Apply each deposit transaction to peerUtxosL1, checking TTL
            // Also collect deposit UTXO IDs (output 0 of deposit txs)
            val newState =
                depositsAug.foldLeft(state) { case (acc, (eventId, signedTx, registerDepositCmd)) =>
                    val validityEnd =
                        registerDepositCmd.depositRefundTxSeq.depositTx.validityEnd
                    val depositTxId = registerDepositCmd.depositRefundTxSeq.depositTx.tx.id
                    val isExpired = acc.currentTime.instant > validityEnd
                    val depositUtxoId = TransactionInput(signedTx.id, 0)

                    // Don't mutate state for expired deposits
                    if isExpired then
                        logger.info(
                          s"Deposit $eventId, tx hash $depositTxId, is expired: " +
                              s"(validityEnd=$validityEnd, currentTime=${acc.currentTime.instant}), not applying L1 tx"
                        )
                        acc
                    else
                        logger.trace(s"Stepping model: submit deposit tx for event ID: $eventId")
                        acc.copy(
                          depositSubmitted = acc.depositSubmitted :+ eventId,
                          depositUtxoIds = acc.depositUtxoIds + depositUtxoId
                        ).applyContinuingL1Tx(signedTx)
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
