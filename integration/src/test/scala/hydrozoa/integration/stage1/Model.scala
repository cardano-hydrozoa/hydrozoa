package hydrozoa.integration.stage1

import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime, FallbackTxStartTime}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.RequestValidityEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.stage1.Commands.*
import hydrozoa.integration.stage1.Model.Error.UnexpectedState
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.given_Ordering_QuantizedInstant.mkOrderingOps
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{RequestValidityEndTimeRaw, UserRequestWithId}
import hydrozoa.multisig.ledger.block.BlockBrief.{Final, Major, Minor}
import hydrozoa.multisig.ledger.block.{BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.eutxol2.tx.L2Genesis.mkGenesisId
import hydrozoa.multisig.ledger.eutxol2.tx.{GenesisObligation, L2Genesis, L2Tx, genesisObligationDecoder}
import hydrozoa.multisig.ledger.eutxol2.{HydrozoaTransactionMutator, toEvacuationKey, toEvacuationMap}
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag.Valid
import hydrozoa.multisig.ledger.event.RequestNumber.increment
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.joint.{EvacuationKey, EvacuationMap, given}
import hydrozoa.multisig.ledger.l1.txseq.DepositRefundTxSeq
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import io.bullet.borer.Cbor
import monocle.Lens
import monocle.syntax.all.focus
import org.scalacheck.commands.ModelCommand
import scalus.cardano.ledger.{AssetName, KeepRaw, SlotConfig, Transaction, TransactionHash, TransactionInput, TransactionOutput, Utxos}

import scala.collection.immutable.{Queue, TreeMap}
import scala.util.chaining.*

object Model:
    private val logger = Logging.logger("Stage1.Model")

    // ===================================
    // Model state
    // ===================================

    /** Model state:
      *   - Should be immutable (scalacheck's requirement)
      *   - Is used for command generation / command application
      *   - Initial state is used for SUT construction.
      */
    case class State(
        //
        // Non-mutable part, always copy as it is, no changes please.
        //
        multiNodeConfig: MultiNodeConfig,

        // Pre-initial state of the peer's L1 utxos.
        // It's needed since [[peerUtxosL1]] reflects the state after applying the initialization tx.
        // This is done upon initial state generation, but maybe there is a better time to run the init tx.
        preinitPeerUtxosL1: Utxos,

        //
        // "Mutable" part
        //
        nextRequestNumber: RequestNumber,
        currentTime: CurrentTime,

        // This is stored here to avoid tossing it over block cycle stages like Done/Ready/InProgress.
        competingFallbackStartTime: FallbackTxStartTime,

        // Block producing cycle
        blockCycle: BlockCycle,

        // I am reverting it back to the Utxos from EvacuationMap, because I don't think
        // we need the evacuation map in this model. The model uses L2 ledger to work with
        // the L2 state, and the evacuation map can be trivially obtained when needed.

        // L2 state
        utxosL2Active: Utxos,

        // L1 state - the only peer's utxos
        peerUtxosL1: Utxos,

        //
        // Deposits (mutable as well)
        //

        // The queue of all generated deposits that Alice wants to register.
        // At all times, all deposits in the list are disjoint in terms of their funding utxo, see [[utxosLocked]].
        depositEnqueued: List[RegisterDepositCommand],
        // Signed deposit transactions - we need them when we submit deposits.
        depositSigned: Map[TransactionHash, Transaction],
        // Utxos used in the deposit enqueued as funding utxos.
        // We need this not to generate deposits that use the same utxos for funding many times.
        utxoLocked: Set[TransactionInput],

        // A subset of depositEnqueued which has been registered by Hydrozoa, i.e.
        // included in a block brief with positive validity flag.
        depositsRegistered: List[RequestId],

        // After a deposit was registered, we may submit it or cancel it depending on
        // how much time is left until the deposit tx's TTL is up - I call it runway.
        // Upon generating [[SubmitDepositsCommand]] we assess whether we have enough
        // runway to take off the deposit - i.e. how much time we have from now to the
        // ttl. This is needed, because the test fails if SUT can't submit deposit tx
        // that model expects to see.
        //
        // So we have two partitions here:
        //  - deposits that have been submitted, so they are expected to appear in the
        // very first block that satisfies their absorption window
        depositSubmitted: List[RequestId],
        //
        //  - deposits, that the model decided not to submit - their funding utxos get
        // unlocked so they can be reused
        depositsDeclined: List[RequestId],
    ) {
        override def toString: String = "<model state (hidden)>"

        def nextRequestId: RequestId =
            RequestId(peerNum = HeadPeerNumber.zero, requestNum = nextRequestNumber)

        /** To save time and keep things simple we exploit the fact that all txs that may mutate the
          * L1 state of the peer's utxo are continuing - they spend the only peer's utxos and pays
          * back at least one utxo that belongs to the peer. So we can always calculate the state of
          * peer's addresses using the preexisting state.
          */
        def applyContinuingL1Tx(l1Tx: Transaction): State = {
            // TODO: this is a bit unwieldy
            // TODO: make a constant?
            // TODO: review
            val peerAddresses = this.peerUtxosL1.map(_._2.address).toSet
                + this.multiNodeConfig.addressOf(HeadPeerNumber.zero)

            val survivedUtxo = this.peerUtxosL1 -- l1Tx.body.value.inputs.toSet
            val newUtxos = survivedUtxo ++ l1Tx.body.value.outputs.toList
                .map(_.value)
                .zipWithIndex
                .filter((output, _) => peerAddresses.contains(output.address))
                .map((output, ix) => TransactionInput(l1Tx.id, ix) -> output)
            this.copy(peerUtxosL1 = newUtxos)
        }

        def depositForSubmission: List[(RequestId, QuantizedInstant)] =
            this.depositEnqueued
                .map(cmd =>
                    cmd.request.requestId -> cmd.depositRefundTxSeq.depositTx.submissionDeadline
                )
                .filter(e => depositsRegistered.contains(e._1))
                .filterNot(e => depositSubmitted.contains(e._1) || depositsDeclined.contains(e._1))
    }

    enum CurrentTime(qi: QuantizedInstant):
        case BeforeHappyPathExpiration(qi: QuantizedInstant) extends CurrentTime(qi)

        case InSilencePeriod(qi: QuantizedInstant) extends CurrentTime(qi)

        case AfterCompetingFallbackStartTime(qi: QuantizedInstant) extends CurrentTime(qi)

        def instant: QuantizedInstant = qi

        def advance(qi: QuantizedInstant): CurrentTime = this match {
            case CurrentTime.BeforeHappyPathExpiration(_) =>
                CurrentTime.BeforeHappyPathExpiration(qi)
            case _ => throw RuntimeException(s"Unexpected current time: $this")
        }

    type BlockAccumulator =
        List[
          (
              // "Unparser" user request
              UserRequestWithId,
              // Parsed counterpart
              L2Tx | DepositUtxo,
              // Validity flag
              ValidityFlag
          )
        ]

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
            // TODO: can we remove it, since it's always current time?
            blockStartTime: QuantizedInstant,
            // We use previous version here since the current version is not defined
            // until the block will be ended (in the future).
            prevVersion: BlockVersion.Full,
            accumulator: BlockAccumulator = List.empty,
        )

        /** The final block is completed. */
        case HeadFinalized

    // Shared lenses for accessing BlockCycle.InProgress and its block accumulator
    private val blockCycleLens: Lens[State, BlockCycle.InProgress] =
        Lens[State, BlockCycle.InProgress](
          get = _.blockCycle.asInstanceOf[BlockCycle.InProgress]
        )(
          replace = bc => s => s.copy(blockCycle = bc)
        )

    private val contentLens
        : Lens[BlockCycle.InProgress, List[(UserRequestWithId, L2Tx | DepositUtxo, ValidityFlag)]] =
        Lens[BlockCycle.InProgress, List[(UserRequestWithId, L2Tx | DepositUtxo, ValidityFlag)]](
          get = _.accumulator
        )(
          replace = events => bc => bc.copy(accumulator = events)
        )

    // ===================================
    // ModelCommand instances
    // ===================================

    // ===================================
    // DelayCommand
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
            val currentTime = cmd.delaySpec match {
                case Delay.EndsBeforeHappyPathExpires(_) =>
                    CurrentTime.BeforeHappyPathExpiration(instant)
                case Delay.EndsInTheSilencePeriod(_) =>
                    CurrentTime.InSilencePeriod(instant)
                case Delay.EndsAfterHappyPathExpires(_) =>
                    CurrentTime.AfterCompetingFallbackStartTime(instant)
            }

            () -> state.copy(
              blockCycle = newBlock,
              currentTime = currentTime
            )
        }

        override def delay(cmd: DelayCommand): scala.concurrent.duration.FiniteDuration =
            cmd.delaySpec.duration.finiteDuration
    }

    // ===================================
    // StartBlockCommand
    // ===================================

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
                              // blockStartTime = cmd.creationTime,
                              blockStartTime = BlockCreationStartTime(state.currentTime.instant),
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

    // ===================================
    // CompleteBlockCommand
    // ===================================

    given ModelCommand[CompleteBlockCommand, BlockBrief, State] with {

        override def runState(
            cmd: CompleteBlockCommand,
            state: State
        ): (BlockBrief, State) = {

            logger.debug(s"MODEL>> CompleteBlockCommand for block number: ${cmd.blockNumber}")

            state.blockCycle match {
                case BlockCycle.InProgress(blockNumber, _creationTime, prevVersion, accumulator) =>

                    logger.trace(
                      s"Completing block ${blockNumber}, accumulator has ${accumulator.length} elements: " +
                          s"with reqeust IDs: ${accumulator.map(_._1.requestId)}"
                    )

                    val (blockBrief, newUtxosL2Active, _utxosWithdrawn) = mkBlockBrief(
                      cmd.blockNumber,
                      accumulator,
                      state.competingFallbackStartTime,
                      state.multiNodeConfig.cardanoNetwork,
                      state.multiNodeConfig.txTiming,
                      BlockCreationStartTime(state.currentTime.instant),
                      cmd.blockCreationEndTime,
                      prevVersion,
                      cmd.isFinal,
                      state.utxosL2Active,
                      state.depositEnqueued,
                      state.depositsRegistered,
                      state.depositSubmitted,
                      state.multiNodeConfig.headConfig.headTokenNames.treasuryTokenName
                    )

                    val newCompetingFallbackStartTime =
                        if blockBrief.isInstanceOf[Major]
                        then
                            val newCompetingFallbackStartTime =
                                state.multiNodeConfig.headConfig.txTiming
                                    .newFallbackStartTime(cmd.blockCreationEndTime)
                            logger.debug(
                              s"newCompetingFallbackStartTime: $newCompetingFallbackStartTime"
                            )
                            newCompetingFallbackStartTime
                        else state.competingFallbackStartTime

                    val nextBlockCycle =
                        if cmd.isFinal then BlockCycle.HeadFinalized
                        else BlockCycle.Done(cmd.blockNumber, blockBrief.blockVersion)

                    // Remove handled, i.e. absorbed and rejected deposits
                    val newDepositsEnqueued = state.depositEnqueued
                        .filterNot(registerDepositCommand =>
                            (blockBrief.depositsAbsorbed ++ blockBrief.depositsRefunded)
                                .contains(
                                  registerDepositCommand.request.requestId
                                )
                        )

                    // Add newly registered deposits
                    val newDepositsRegistered = state.depositsRegistered
                        ++ blockBrief.events
                            .filter(_._2 == Valid)
                            .map(_._1)
                            // TODO: do we need that? Why jsut not all deposits?
                            .filter(e => state.depositEnqueued.map(_.request.requestId).contains(e))

                    val newState = state.copy(
                      blockCycle = nextBlockCycle,
                      competingFallbackStartTime = newCompetingFallbackStartTime,
                      depositEnqueued = newDepositsEnqueued,
                      depositsRegistered = newDepositsRegistered,
                      utxosL2Active = newUtxosL2Active,
                      currentTime = state.currentTime.advance(blockBrief.endTime.convert)
                    )

                    logger.trace(
                      s"block ${cmd.blockNumber}, newState.depositEnqueued=${newState.depositEnqueued}, " +
                          s"newState.depositsRegistered=${newState.depositsRegistered}"
                    )

                    // Return
                    blockBrief -> newState
                case _ =>
                    throw UnexpectedState("CompleteBlockCommand requires BlockCycle.InProgress")
            }
        }

        private def mkBlockBrief(
            blockNumber: BlockNumber,
            accumulator: BlockAccumulator,
            competingFallbackStartTime: FallbackTxStartTime,
            cardanoNetwork: CardanoNetwork.Section,
            txTiming: TxTiming,
            blockStartTime: BlockCreationStartTime,
            blockEndTime: BlockCreationEndTime,
            prevVersion: BlockVersion.Full,
            isFinal: Boolean,
            activeUtxos: Utxos,
            depositEnqueued: List[RegisterDepositCommand],
            depositRegistered: List[RequestId],
            depositsSubmitted: List[RequestId],
            treasuryTokenName: AssetName
        ): (BlockBrief, Utxos, Unit) = {

            logger.trace(s"mkBlockBrief: blockNumber: $blockNumber")
            logger.trace(s"mkBlockBrief: blockStartTime: $blockStartTime")

            val settlementValidityEnd =
                txTiming.newSettlementEndTime(competingFallbackStartTime)
            logger.trace(s"settlementValidityEnd=$settlementValidityEnd")

            val requestValidity = accumulator.map((le, _, flag) => le.requestId -> flag)

            val depositsToCheck =
                depositEnqueued.filter(d => depositRegistered.contains(d.request.requestId))

            val depositsAbsorbed = depositsToCheck
                .filter(registerDepositCommand => {

                    // TODO: this should be done during parsing I guess?
                    val requestValidityEnd = RequestValidityEndTime(
                      cardanoNetwork.slotConfig,
                      registerDepositCommand.request.request.header.validityEnd
                    )
                    val depositAbsorptionStart =
                        txTiming.depositAbsorptionStartTime(requestValidityEnd)
                    val depositAbsorptionEnd =
                        txTiming.depositAbsorptionEndTime(requestValidityEnd)

                    val isDepositUtxoFound =
                        depositsSubmitted.contains(registerDepositCommand.request.requestId)

                    logger.trace(
                      s"MODEL deposit absorption check: ${registerDepositCommand.request.requestId},\n" +
                          s"depositAbsorptionStart=$depositAbsorptionStart, " +
                          s"depositAbsorptionEnd=$depositAbsorptionEnd, " +
                          s"utxo is found: $isDepositUtxoFound"
                    )

                    // Check all the conditions
                    depositAbsorptionStart.convert <= blockStartTime
                    && depositAbsorptionEnd.convert >= settlementValidityEnd.convert
                    && isDepositUtxoFound
                })
                .map(_._1.requestId)

            logger.trace(s"depositsAbsorbed: $depositsAbsorbed")

            val depositsRefunded =
                (if isFinal
                 then depositsToCheck // all known deposits should be refunded
                 else
                     depositsToCheck.filter(registerDepositCommand =>

                         // TODO: this should be done during parsing I guess?
                         val requestValidityEnd = RequestValidityEndTime(
                           cardanoNetwork.slotConfig,
                           registerDepositCommand.request.request.header.validityEnd
                         )
                         val depositAbsorptionStart =
                             txTiming.depositAbsorptionStartTime(requestValidityEnd)
                         val depositAbsorptionEnd =
                             txTiming.depositAbsorptionEndTime(requestValidityEnd)
                         val settlementValidityEnd =
                             txTiming.newSettlementEndTime(competingFallbackStartTime)
                         val isDepositUtxoFound =
                             depositsSubmitted.contains(registerDepositCommand.request.requestId)

                         logger.trace(
                           s"MODEL deposit rejection check: ${registerDepositCommand.request.requestId},\n" +
                               s"depositAbsorptionEnd=$depositAbsorptionEnd, " +
                               s"settlementValidityEnd=$settlementValidityEnd"
                         )

                         settlementValidityEnd.convert > depositAbsorptionEnd.convert
                         || (depositAbsorptionStart.convert < blockStartTime && !isDepositUtxoFound)
                     )
                ).map(_._1.requestId)

            logger.trace(s"depositsRefunded: $depositsRefunded")

            // An "L2 genesis" is what we now call deposit compartment, keeping them for now.
            val depositCompartments: List[L2Genesis] =
                depositsAbsorbed
                    .map(requestId =>
                        depositEnqueued
                            .find(_.request.requestId == requestId)
                            .getOrElse(
                              throw RuntimeException(s"deposit not found: $requestId")
                            )
                    )
                    .map(rdc => {
                        val l2Payload: Array[Byte] =
                            rdc.depositRefundTxSeq.depositTx.depositProduced.l2Payload.bytes
                        val obligations =
                            Cbor.decode(l2Payload).to[Queue[GenesisObligation]].value.toList
                        val genesisId =
                            mkGenesisId(rdc.depositRefundTxSeq.depositTx.depositProduced.utxoId)
                        L2Genesis(Queue.from(obligations), genesisId)
                    })

            val newActiveUtxos =
                activeUtxos ++ depositCompartments.flatMap(_.asUtxos.map((i, o) => i -> o.value))
            val newEvacuationMap = newActiveUtxos
                .toEvacuationMap(cardanoNetwork)
                .toOption
                .getOrElse(throw RuntimeException("cannot build the evacuation map"))

            lazy val newFallbackTxStartTime = txTiming.newFallbackStartTime(blockEndTime)
            lazy val newForcedMajorBlockTime = txTiming.forcedMajorBlockTime(newFallbackTxStartTime)
            lazy val newMajorBlockWakeupTime =
                TxTiming.majorBlockWakeupTime(forcedMajorBlockTime, None)

            lazy val majorBlock = Major(
              header = BlockHeader.Major(
                blockNum = blockNumber,
                blockVersion = prevVersion.incrementMajor,
                startTime = blockStartTime,
                endTime = blockEndTime,
                kzgCommitment = newEvacuationMap.kzgCommitment,
                fallbackTxStartTime = newFallbackTxStartTime,
                majorBlockWakeupTime = newMajorBlockWakeupTime
              ),
              body = BlockBody.Major(
                events = requestValidity,
                depositsAbsorbed = depositsAbsorbed,
                depositsRefunded = depositsRefunded
              )
            )

            lazy val forcedMajorBlockTime =
                txTiming.forcedMajorBlockTime(competingFallbackStartTime)
            lazy val majorBlockWakeupTime =
                TxTiming.majorBlockWakeupTime(forcedMajorBlockTime, None)

            lazy val minorBlock = Minor(
              header = BlockHeader.Minor(
                blockNum = blockNumber,
                blockVersion = prevVersion.incrementMinor,
                startTime = blockStartTime,
                endTime = blockEndTime,
                kzgCommitment = newEvacuationMap.kzgCommitment,
                fallbackTxStartTime = competingFallbackStartTime, // doesn't change
                majorBlockWakeupTime = majorBlockWakeupTime // doesn't change
              ),
              body = BlockBody.Minor(
                events = accumulator.map((le, _, flag) => le.requestId -> flag),
                depositsRefunded = depositsRefunded
              )
            )

            lazy val finalBlock = Final(
              header = BlockHeader.Final(
                blockNum = blockNumber,
                blockVersion = prevVersion.incrementMajor,
                startTime = blockStartTime,
                endTime = blockEndTime,
              ),
              body = BlockBody.Final(
                events = accumulator.map((le, _, flag) => le.requestId -> flag),
                depositsRefunded = depositsRefunded
              )
            )

            val brief =
                if isFinal then finalBlock
                else if txTiming.blockCanStayMinor(
                      blockEndTime,
                      competingFallbackStartTime
                    )
                then {
                    val hasWithdrawals = accumulator.exists(_._2 match {
                        case e: L2Tx => e.l1utxos.nonEmpty
                        case _       => false
                    })
                    val hasDepositsAbsorbed: Boolean = depositsAbsorbed.nonEmpty

                    if hasWithdrawals || hasDepositsAbsorbed
                    then majorBlock
                    else minorBlock
                } else majorBlock

            logger.trace(s"block brief: $brief")

            // The idea was to reuse withdrawn utxos on L1.
            // The problem with that is that the model knows nothing about the effects.
            // As a consequence, we don't know utxo ids of withdrawn funds, and we cannot reuse them.
            val withdrawnUtxos: Unit = ()

            (brief, newActiveUtxos, withdrawnUtxos)
        }

        override def preCondition(cmd: CompleteBlockCommand, state: State): Boolean =
            state.blockCycle match {
                case BlockCycle.InProgress(currentBlockNumber, _, _, _) =>
                    cmd.blockNumber == currentBlockNumber
                case _ => false
            }
    }

    // ===================================
    // L2TxCommand
    // ===================================

    given ModelCommand[L2TxCommand, Unit, State] with {

        override def runState(cmd: L2TxCommand, state: State): (Unit, State) =

            logger.debug(s"MODEL>> L2TxCommand for event ID: ${cmd.request.requestId}")

            val BlockCycle.InProgress(_, _, _, currentEvents) = state.blockCycle: @unchecked
            logger.trace(s"INPUT state.blockCycle event IDs: ${currentEvents.map(_._1.requestId)}")

            val l2Tx: L2Tx = L2Tx
                .parse(
                  cmd.request.request.body.l2Payload.bytes,
                  state.multiNodeConfig.cardanoNetwork
                )
                .fold(err => throw RuntimeException(s"Failed to parse L2Tx: $err"), identity)

            val ret = HydrozoaTransactionMutator.transit(
              config = state.multiNodeConfig.headConfig,
              time = state.currentTime.instant,
              state = state.utxosL2Active,
              l2Tx = l2Tx
            )

            val newState = ret match {
                case Left(err) =>
                    logger.debug(s"invalid L2 tx ${cmd.request.requestId}: ${err}")
                    blockCycleLens
                        .andThen(contentLens)
                        .modify(_ :+ (cmd.request, l2Tx, ValidityFlag.Invalid))(state)
                case Right(mutatorState) =>
                    state
                        .pipe(
                          blockCycleLens
                              .andThen(contentLens)
                              .modify(_ :+ (cmd.request, l2Tx, ValidityFlag.Valid))
                        )
                        .focus(_.utxosL2Active)
                        .replace(mutatorState)
            }

            val finalState = newState
                .focus(_.nextRequestNumber)
                .modify(_.increment)

            val BlockCycle.InProgress(_, _, _, finalEvents) = finalState.blockCycle: @unchecked

            logger.trace(
              s"OUTPUT finalState.blockCycle event IDs: ${finalEvents.map(_._1.requestId)}"
            )

            () -> finalState
    }

    // ===================================
    // RegisterDepositCommand
    // ===================================

    given ModelCommand[RegisterDepositCommand, Unit, State] with {
        override def runState(
            cmd: RegisterDepositCommand,
            state: State
        ): (Unit, State) = {

            import cmd.request as req
        import state.multiNodeConfig as config

            logger.debug(
              s"MODEL>> RegisterDepositCommand for event ID: ${cmd.request.requestId}"
            )

            val BlockCycle.InProgress(_, blockStartTime, _, currentEvents) =
                state.blockCycle: @unchecked
            logger.trace(s"INPUT state.blockCycle event IDs: ${currentEvents.map(_._1.requestId)}")

            val requestValidityEndTime = RequestValidityEndTime(
                config.slotConfig,
                req.request.header.validityEnd
            )
            val seq =
                DepositRefundTxSeq
                    .Parse(config.headConfig)(
                      depositTxBytes = req.request.body.l1Payload,
                      l2Payload = req.request.body.l2Payload,
                      requestId = req.requestId,
                      requestValidityEndTime = requestValidityEndTime
                    )
                    .result
                    .fold(e => throw RuntimeException(e), identity)

            // For now, all deposits request should be valid by construction
            require(blockStartTime < seq.depositTx.submissionDeadline)

            val depositAbsorptionEndTime = config.headConfig.txTiming.depositAbsorptionEndTime(
                requestValidityEndTime
            )
            require(
              blockStartTime < depositAbsorptionEndTime
            )

            logger.trace(s"deposit txHash=${seq.depositTx.tx.id}")

            val depositUtxo = seq.depositTx.depositProduced

            val newState = state
                .pipe(
                  blockCycleLens
                      .andThen(contentLens)
                      .modify(_ :+ (cmd.request, depositUtxo, ValidityFlag.Valid))
                )
                .focus(_.depositEnqueued)
                .modify(_ :+ cmd)
                .focus(_.utxoLocked)
                .modify(_ ++ seq.depositTx.tx.body.value.inputs.toSeq)
                .focus(_.depositSigned)
                .modify(_ + (cmd.depositTxBytesSigned.id -> cmd.depositTxBytesSigned))
                .focus(_.nextRequestNumber)
                .modify(_.increment)

            val BlockCycle.InProgress(_, _, _, finalEvents) = newState.blockCycle: @unchecked

            logger.trace(
              s"OUTPUT newState.blockCycle event IDs: ${finalEvents.map(_._1.requestId)}"
            )
            logger.trace(
              s"OUTPUT newState.depositEnqueued IDs and submission deadlines: ${newState.depositEnqueued
                      .map(e => e._1.requestId -> e._2.depositTx.submissionDeadline)}"
            )

            () -> newState
        }
    }

    // ===================================
    // SubmitDepositsCommand
    // ===================================

    given ModelCommand[SubmitDepositsCommand, Unit, State] with {
        override def runState(
            cmd: SubmitDepositsCommand,
            state: State
        ): (Unit, State) = {
            logger.debug(
              s"MODEL>> SubmitDepositCommand, for submission: ${cmd.depositsForSubmission.size}, " +
                  s"for rejection: ${cmd.depositsForRejection.size}"
            )

            val allDeposits = cmd.depositsForSubmission.map(e => true -> e._1)
                ++ cmd.depositsForRejection.map(e => false -> e)

            // Attach corresponding register deposit commands
            val depositsAug = allDeposits.map((flag, eventId) =>
                val registerDepositCmd = state.depositEnqueued
                    .find(_.request.requestId == eventId)
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
                          depositsDeclined = acc.depositsDeclined :+ eventId
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
