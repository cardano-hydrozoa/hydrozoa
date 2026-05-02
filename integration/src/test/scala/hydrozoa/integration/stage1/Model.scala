package hydrozoa.integration.stage1

import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime, FallbackTxStartTime, MajorBlockWakeupTime, SettlementTxEndTime}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.stage1.Commands.*
import hydrozoa.integration.stage1.Model.Error.UnexpectedState
import hydrozoa.integration.stage1.model.Deposits
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.given_Ordering_QuantizedInstant.mkOrderingOps
import hydrozoa.lib.logging.Logging
import cats.syntax.all.*
import cats.*
import hydrozoa.integration.stage1.Model.logger
import hydrozoa.integration.stage1.model.Deposits.DepositStatus.*
import hydrozoa.integration.stage1.model.Deposits.depositAbsorptionStart
import scalus.|>
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.UserRequestWithId
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
        deposits: Deposits,

        // Utxos used in the deposit enqueued as funding utxos.
        // We need this not to generate deposits that use the same utxos for funding many times.
        utxoLocked: Set[TransactionInput],
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
            // cats.State[State, BlockBrief]
            logger.debug(s"MODEL>> CompleteBlockCommand for block number: ${cmd.blockNumber}")
            state.blockCycle match {

                case BlockCycle.InProgress(blockNumber, _creationTime, prevVersion, accumulator) =>

                    logger.trace(
                      s"Completing block ${blockNumber}, accumulator has ${accumulator.length} elements: " +
                          s"with reqeust IDs: ${accumulator.map(_._1.requestId)}"
                    )

                    val events: List[(RequestId, ValidityFlag)] =
                        accumulator.map((le, _, flag) => le.requestId -> flag)

                    val s: cats.data.State[State, BlockBrief] =
                        for {
                            regOrReject <- registerOrReject(events)
                            registeredThisBlock = regOrReject._1
                            rejectedThisBlock = regOrReject._2

                            absorbedThisBlock <- absorb

                            refundedThisBlock <- refund(cmd.isFinal)

                            newEvacuationMap <- updateEvacuationMap

                            blockBrief <- mkBlockBrief(
                              cmd.isFinal,
                              cmd.blockCreationEndTime,
                              cmd.blockNumber,
                              prevVersion,
                              newEvacuationMap,
                              accumulator,
                              absorbedThisBlock,
                              refundedThisBlock
                            )

                            // update block cycle
                            _ <- cats.data.State.modify[State](state => {
                                val nextBlockCycle =
                                    if cmd.isFinal then BlockCycle.HeadFinalized
                                    else BlockCycle.Done(cmd.blockNumber, blockBrief.blockVersion)
                                state.copy(blockCycle = nextBlockCycle)
                            })
                            // tick time
                            _ <- cats.data.State.modify[State](state =>
                                state.copy(currentTime =
                                    state.currentTime.advance(blockBrief.endTime.convert)
                                )
                            )
                        } yield blockBrief

                    s.run(state).swapF.value
                case _ =>
                    throw UnexpectedState("CompleteBlockCommand requires BlockCycle.InProgress")
            }
        }

        private def liftEndo(endo: State => State): cats.data.State[State, Unit] =
            cats.data.State[State, Unit](s => (endo(s), ()))

        private def getBlockCreationStartTime: cats.data.State[State, BlockCreationStartTime] =
            cats.data.State.get.map(state => BlockCreationStartTime(state.currentTime.instant))

        private def getSettlementValidityEnd: cats.data.State[State, SettlementTxEndTime] =
            cats.data.State.get.map(state =>
                state.multiNodeConfig.txTiming.newSettlementEndTime(
                  state.competingFallbackStartTime
                )
            )

        private def getMajorBlockWakeupTime(
            fallbackTxStartTime: FallbackTxStartTime
        ): cats.data.State[State, MajorBlockWakeupTime] =
            for {
                state <- cats.data.State.get[State]
                newForcedMajorBlockTime =
                    state.multiNodeConfig.txTiming.forcedMajorBlockTime(fallbackTxStartTime)
                mAbsorptionStartTime: Option[DepositAbsorptionStartTime] =
                    state.deposits.mAbsorptionStartTime(using state.multiNodeConfig)
                wakeupTime =
                    TxTiming.majorBlockWakeupTime(newForcedMajorBlockTime, mAbsorptionStartTime)

            } yield wakeupTime

        /** Register or reject [[Enqueued]] deposits depending on their [[ValidityFlag]], as derived
          * from the [[BlockAccumulator]]
          * @param events
          * @return
          *   a tuple of (registeredEvents, rejectedEvents)
          */
        private def registerOrReject(
            events: List[(RequestId, ValidityFlag)]
        ): cats.data.State[State, (Queue[Enqueued], Queue[Enqueued])] = for {
            state <- cats.data.State.get[State]
            // FIXME: this could be done probably in a single fold, and perhaps more performant for large
            // lists of events by using a `Map[RequestId, ValidityFlag]` instead.
            // Or perhaps the accumulator should just store the full RegisterDepositCommand,
            //
            // I'm also not 100% certain how the accumulator gets populated right now -- is it meaningfully
            // distinct from the new semantics of the Deposits.depositsEnqueued?
            requestsInAccumulator: Queue[(ValidityFlag, Enqueued)] = {
                // Look at all enqueued deposits
                state.deposits.depositsEnqueued
                    // map them with the validity flag, if its in the accumulator
                    .map(cmd =>
                        val thisEvent: Option[(RequestId, ValidityFlag)] =
                            events.find(event => event._1 == cmd.request.requestId)
                        thisEvent.map((requestId, validityFlag) => (validityFlag, cmd))
                    )
                    // Then filter out all the requests not in the accumulator
                    .filter(_.isDefined)
                    .map(_.get)

            }
            depositsToRegisterOrReject: (
                Queue[Enqueued],
                Queue[Enqueued]
            ) = requestsInAccumulator
                .partition(_._1 == Valid)
                .bimap(_.map(_._2), _.map(_._2))
            _ <- liftEndo(Deposits.register(depositsToRegisterOrReject._1))
            _ <- liftEndo(Deposits.reject(depositsToRegisterOrReject._2))
        } yield depositsToRegisterOrReject

        // TODO: Don't we need to limit the max number we absorb? That's not being done here
        private val absorb: cats.data.State[State, Queue[Submitted]] = for {
            state <- cats.data.State.get[State]
            blockStartTime <- getBlockCreationStartTime
            settlementValidityEnd <- getSettlementValidityEnd

            depositsToAbsorb: Queue[Submitted] = state.deposits.depositsSubmitted
                .filter(submitted => {
                    given TxTiming.Section = state.multiNodeConfig

                    logger.trace(
                      s"MODEL deposit absorption check: ${submitted.request.requestId},\n" +
                          s"depositAbsorptionStart=${submitted.depositAbsorptionStart}, " +
                          s"depositAbsorptionEnd=${submitted.depositAbsorptionEnd}"
                    )

                    // Check all the conditions
                    submitted.depositAbsorptionStart.convert <= blockStartTime
                    && submitted.depositAbsorptionEnd.convert >= settlementValidityEnd.convert
                })

            _ = logger.trace(s"depositsToAbsorb: $depositsToAbsorb")

            _ <- liftEndo(Deposits.absorb(depositsToAbsorb))
        } yield depositsToAbsorb

        private def refund(
            isFinal: Boolean
        ): cats.data.State[State, Queue[Refundable]] = for {
            state <- cats.data.State.get[State]
            blockStartTime <- getBlockCreationStartTime
            settlementValidityEnd <- getSettlementValidityEnd

            depositsToRefund: Queue[Refundable] =
                if isFinal
                then
                    state.deposits.hydrozoaKnownRegisteredDeposits // all known deposits should be refunded
                else
                    state.deposits.hydrozoaKnownRegisteredDeposits.filter(refundable =>
                        given TxTiming.Section = state.multiNodeConfig

                        logger.trace(
                          s"MODEL deposit refund check: ${refundable.request.requestId},\n" +
                              s"depositAbsorptionEnd=$refundable.depositAbsorptionEnd, " +
                              s"settlementValidityEnd=$settlementValidityEnd"
                        )

                        settlementValidityEnd.convert > refundable.depositAbsorptionEnd.convert
                        || (refundable.depositAbsorptionStart.convert < blockStartTime)
                    )
            _ = logger.trace(s"depositToRefund: $depositsToRefund")
            _ <- liftEndo(Deposits.refund(depositsToRefund))
        } yield depositsToRefund

        private val updateEvacuationMap: cats.data.State[State, EvacuationMap] =
            for {
                state <- cats.data.State.get[State]
                // An "L2 genesis" is what we now call deposit compartment, keeping them for now.
                depositCompartments: Queue[L2Genesis] =
                    state.deposits.depositsAbsorbed
                        .map(rdc => {
                            val l2Payload: Array[Byte] =
                                rdc.depositRefundTxSeq.depositTx.depositProduced.l2Payload.bytes
                            val obligations =
                                Cbor.decode(l2Payload).to[Queue[GenesisObligation]].value.toList
                            val genesisId =
                                mkGenesisId(
                                  rdc.depositRefundTxSeq.depositTx.depositProduced.utxoId
                                )
                            L2Genesis(Queue.from(obligations), genesisId)
                        })

                newActiveUtxos =
                    state.utxosL2Active ++ depositCompartments.flatMap(
                      _.asUtxos.map((i, o) => i -> o.value)
                    )

                _ <- cats.data.State.set[State](state.copy(utxosL2Active = newActiveUtxos))

                newEvacuationMap = newActiveUtxos
                    .toEvacuationMap(state.multiNodeConfig)
                    .toOption
                    .getOrElse(throw RuntimeException("cannot build the evacuation map"))
            } yield newEvacuationMap

        private def majorBlock(
            blockEndTime: BlockCreationEndTime,
            blockNumber: BlockNumber,
            blockVersion: BlockVersion.Full,
            newEvacuationMap: EvacuationMap,
            events: List[(RequestId, ValidityFlag)],
            absorbedThisBlock: Queue[Submitted],
            refundedThisBlock: Queue[Refundable]
        ): cats.data.State[State, BlockBrief.Major] =
            for {
                state <- cats.data.State.get[State]
                txTiming = state.multiNodeConfig.txTiming

                blockStartTime <- getBlockCreationStartTime

                newFallbackTxStartTime = txTiming.newFallbackStartTime(blockEndTime)
                newMajorBlockWakeupTime <- getMajorBlockWakeupTime(newFallbackTxStartTime)

                majorBlock = Major(
                  header = BlockHeader.Major(
                    blockNum = blockNumber,
                    blockVersion = blockVersion,
                    startTime = blockStartTime,
                    endTime = blockEndTime,
                    kzgCommitment = newEvacuationMap.kzgCommitment,
                    fallbackTxStartTime = newFallbackTxStartTime,
                    majorBlockWakeupTime = newMajorBlockWakeupTime
                  ),
                  body = BlockBody.Major(
                    events = events,
                    depositsAbsorbed = absorbedThisBlock.map(_.cmd.request.requestId).toList,
                    depositsRefunded = refundedThisBlock.map(_.cmd.request.requestId).toList
                  )
                )

                _ <- cats.data.State.modify[State](
                  _.copy(competingFallbackStartTime = newFallbackTxStartTime)
                )
                _ = logger.debug(
                  s"newCompetingFallbackStartTime: $newFallbackTxStartTime"
                )

            } yield majorBlock

        private def minorBlock(
            blockEndTime: BlockCreationEndTime,
            blockNumber: BlockNumber,
            blockVersion: BlockVersion.Full,
            newEvacuationMap: EvacuationMap,
            events: List[(RequestId, ValidityFlag)],
            refundedThisBlock: Queue[Refundable]
        ): cats.data.State[State, BlockBrief.Minor] = for {
            state <- cats.data.State.get[State]
            blockStartTime = BlockCreationStartTime(state.currentTime.instant)
            majorBlockWakeupTime <- getMajorBlockWakeupTime(state.competingFallbackStartTime)
            minorBlock = Minor(
              header = BlockHeader.Minor(
                blockNum = blockNumber,
                blockVersion = blockVersion,
                startTime = blockStartTime,
                endTime = blockEndTime,
                kzgCommitment = newEvacuationMap.kzgCommitment,
                fallbackTxStartTime = state.competingFallbackStartTime, // doesn't change
                majorBlockWakeupTime = majorBlockWakeupTime // doesn't change
              ),
              body = BlockBody.Minor(
                events = events,
                depositsRefunded = refundedThisBlock.map(_.cmd.request.requestId).toList
              )
            )
        } yield minorBlock

        def finalBlock(
            blockEndTime: BlockCreationEndTime,
            blockNumber: BlockNumber,
            blockVersion: BlockVersion.Full,
            events: List[(RequestId, ValidityFlag)],
            refundedThisBlock: Queue[Refundable]
        ): cats.data.State[State, BlockBrief.Final] = for {
            _ <- cats.data.State
                .modify[State](_.copy(blockCycle = BlockCycle.HeadFinalized))
            blockStartTime <- getBlockCreationStartTime

            finalBlock = Final(
              header = BlockHeader.Final(
                blockNum = blockNumber,
                blockVersion = blockVersion,
                startTime = blockStartTime,
                endTime = blockEndTime,
              ),
              body = BlockBody.Final(
                events = events,
                depositsRefunded = refundedThisBlock.map(_.cmd.request.requestId).toList
              )
            )

        } yield finalBlock

        private def mkBlockBrief(
            isFinal: Boolean,
            blockEndTime: BlockCreationEndTime,
            blockNumber: BlockNumber,
            prevVersion: BlockVersion.Full,
            newEvacuationMap: EvacuationMap,
            accumulator: BlockAccumulator,
            absorbedThisBlock: Queue[Submitted],
            refundedThisBlock: Queue[Refundable]
        ): cats.data.State[State, BlockBrief] =
            for {
                state <- cats.data.State.get[State]

                events = accumulator.map((req, _, flag) => (req.requestId, flag))

                // Construct, but don't execute the state transitions -- we decide which one we need below
                doMajorBlock = majorBlock(
                  blockEndTime,
                  blockNumber,
                  prevVersion.incrementMajor,
                  newEvacuationMap,
                  events,
                  absorbedThisBlock,
                  refundedThisBlock
                )
                doMinorBlock = minorBlock(
                  blockEndTime,
                  blockNumber,
                  prevVersion.incrementMinor,
                  newEvacuationMap,
                  events,
                  refundedThisBlock
                )
                doFinalBlock = finalBlock(
                  blockEndTime,
                  blockNumber,
                  prevVersion.incrementMajor,
                  events,
                  refundedThisBlock
                )

                blockCanStayMinor = state.multiNodeConfig.txTiming.blockCanStayMinor(
                  blockEndTime,
                  state.competingFallbackStartTime
                )

                hasWithdrawals = accumulator.exists(_._2 match {
                    case e: L2Tx => e.l1utxos.nonEmpty
                    case _       => false
                })
                hasDepositsAbsorbed: Boolean = absorbedThisBlock.nonEmpty

                // I wanted to do this with pattern guards in a case expression, but the type checker
                // complained
                brief: BlockBrief <-
                    if isFinal
                    then doFinalBlock
                    else if blockCanStayMinor
                    then {
                        if hasWithdrawals || hasDepositsAbsorbed
                        then doMajorBlock
                        else doMinorBlock
                    } else doMajorBlock

                _ = logger.trace(s"block brief: $brief")
            } yield brief

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

            val requestValidityEndTime = req.request.header.validityEnd

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

            val newState: State = state
                .pipe(
                  blockCycleLens
                      .andThen(contentLens)
                      .modify(_ :+ (cmd.request, depositUtxo, ValidityFlag.Valid))
                )
                .pipe(Deposits.enqueue(Queue(cmd)))
                .focus(_.utxoLocked)
                .modify(_ ++ seq.depositTx.tx.body.value.inputs.toSeq)
                .focus(_.nextRequestNumber)
                .modify(_.increment)

            val BlockCycle.InProgress(_, _, _, finalEvents) = newState.blockCycle: @unchecked

            logger.trace(
              s"OUTPUT newState.blockCycle event IDs: ${finalEvents.map(_._1.requestId)}"
            )
            logger.trace(
              s"OUTPUT newState.depositEnqueued IDs and submission deadlines: ${newState.deposits.depositsEnqueued
                      .map(e =>
                          e.request.requestId -> e.cmd.depositRefundTxSeq.depositTx.submissionDeadline
                      )}"
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
                  s"for rejection: ${cmd.depositsToDecline.size}"
            )

            // TODO: Move this into the Deposits.submit method
            val applyContinuingL1TxEndo: (Transaction => State => State) = tx =>
                original => original.applyContinuingL1Tx(tx)

            // TODO: move this into the Deposits.decline method
            val unlockUtxosEndo: (Set[TransactionInput] => State => State) = inputs =>
                original => original.focus(_.utxoLocked).modify(_ -- inputs)

            // TODO: If we switch to the state monad, this can be done with traverse
            val endos: Queue[State => State] =
                cmd.depositsForSubmission
                    .map(registered => applyContinuingL1TxEndo(registered.depositTxBytesSigned))
                    .appended(Deposits.submit(cmd.depositsForSubmission))
                    .appended(Deposits.decline(cmd.depositsToDecline))
                    .appendedAll(
                      cmd.depositsToDecline.map(registered =>
                          unlockUtxosEndo(registered.depositTxBytesSigned.body.value.inputs.toSet)
                      )
                    )

            val newState = endos.foldLeft(state)(_ |> _)
            () -> newState
        }
    }

    enum Error extends Throwable:
        case UnexpectedState(msg: String)

        override def getMessage: String = this match {
            case Error.UnexpectedState(msg) => s"Unexpected state while stepping the model: $msg"
        }

end Model
