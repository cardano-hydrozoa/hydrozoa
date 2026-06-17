package hydrozoa.integration.stage1

import cats.*
import cats.data.StateT
import cats.syntax.all.*
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.*
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.stage1.Commands.*
import hydrozoa.integration.stage1.Model.CurrentTime.BeforeHappyPathExpiration
import hydrozoa.integration.stage1.Model.Error.UnexpectedState
import hydrozoa.integration.stage1.model.Deposits
import hydrozoa.integration.stage1.model.Deposits.DepositStatus.*
import hydrozoa.integration.stage1.model.Deposits.{DepositStatus, depositAbsorptionStart}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.given_Ordering_QuantizedInstant.mkOrderingOps
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, debug, warn}
import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockBrief.{Final, Major, Minor}
import hydrozoa.multisig.ledger.block.*
import hydrozoa.multisig.ledger.eutxol2.HydrozoaTransactionMutator
import hydrozoa.multisig.ledger.eutxol2.tx.L2Tx
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag.Valid
import hydrozoa.multisig.ledger.event.RequestNumber.increment
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.l1.txseq.DepositRefundTxSeq
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import monocle.Lens
import monocle.syntax.all.focus
import org.scalacheck.commands.ModelCommand
import scalus.cardano.ledger.{BlockHeader as _, *}

import scala.collection.immutable.Queue
import scala.concurrent.duration.FiniteDuration
import scala.util.chaining.*

object Model:
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
        //
        reservedSubmissionDuration: FiniteDuration,

        // The real-world instant at which the SUT should start processing commands.
        // None for mock mode (time is controlled via TestControl).
        // Some(t) for Yaci/public: startupSut sleeps until t, or aborts if t is already past.
        takeoffTime: Option[java.time.Instant],

        // Pre-initial state of the peer's L1 utxos.
        // It's needed since [[peerUtxosL1]] reflects the state after applying the initialization tx.
        // This is done upon initial state generation, but maybe there is a better time to run the init tx.
        preinitPeerUtxosL1: Utxos,

        //
        // "Mutable" part
        //
        nextRequestNumber: RequestNumber,
        private val currentTime: CurrentTime,

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

        /** Calculate the current major block wakeup time according to the current state of the
          * deposits. This is the earliest DepositAbsorptionStateTime of any deposit that hydrozoa
          * will check for absorption
         * */
        def mAbsorptionStartTime: Option[DepositAbsorptionStartTime] = {
            val absorptionBounds: Queue[(DepositAbsorptionStartTime, DepositAbsorptionEndTime)] =
                deposits.hydrozoaKnownRegisteredDeposits.map(cmd =>
                    val validityEnd = cmd.request.request.header.validityEnd
                    (
                      multiNodeConfig.txTiming.depositAbsorptionStartTime(validityEnd),
                      multiNodeConfig.txTiming.depositAbsorptionEndTime(validityEnd)
                    )
                )

            absorptionBounds.map(_._1).minOption
        }

        def getCurrentTime: CurrentTime = currentTime

        def advanceCurrentTime(quantizedInstant: QuantizedInstant): State = {
            val newTime: CurrentTime = quantizedInstant match {
                case _ if quantizedInstant >= this.competingFallbackStartTime =>
                    CurrentTime.AfterCompetingFallbackStartTime(quantizedInstant)
                case _
                    if this.competingFallbackStartTime - this.multiNodeConfig.txTiming.silenceDuration <= quantizedInstant =>
                    CurrentTime.InSilencePeriod(quantizedInstant)
                case _ => BeforeHappyPathExpiration(quantizedInstant)
            }
            this.copy(currentTime = newTime)
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

    private def liftS[M[_]: Applicative, A](s: cats.data.State[State, A]): StateT[M, State, A] =
        s.transformF(_.value.pure[M])

    // ===================================
    // ModelCommand instances
    // ===================================

    // ===================================
    // DelayCommand
    // ===================================

    given ModelCommand[DelayCommand, Unit, State] with {

        override def runState[M[_]: MonadThrow](
            cmd: DelayCommand
        )(using log: ContraTracer[M, Slf4jMsg]): StateT[M, State, Unit] =
            StateT.modifyF[M, State] { state =>
                val newBlockM = state.blockCycle match {
                    case BlockCycle.Done(blockNumber, version) =>
                        MonadThrow[M].pure(BlockCycle.Ready(blockNumber = blockNumber, prevVersion = version))
                    case _ =>
                        MonadThrow[M].raiseError(
                          Error.UnexpectedState("DelayCommand requires BlockCycle.Done")
                        )
                }
                for
                    newBlock <- newBlockM
                    instant = state.getCurrentTime.instant + cmd.delaySpec.duration
                    newState = state.copy(blockCycle = newBlock).advanceCurrentTime(instant)
                    _ <- log.debug(s"MODEL>> DelayCommand: ${cmd.delaySpec}")
                    _ <- newState.getCurrentTime match {
                        case CurrentTime.InSilencePeriod(_) |
                            CurrentTime.AfterCompetingFallbackStartTime(_) =>
                            log.warn(s"MODEL>> DelayCommand: model time entering silence/fallback zone")
                        case _ => MonadThrow[M].pure(())
                    }
                yield newState
            }

        override def delay(cmd: DelayCommand): scala.concurrent.duration.FiniteDuration =
            cmd.delaySpec.duration.finiteDuration
    }

    // ===================================
    // StartBlockCommand
    // ===================================

    given ModelCommand[StartBlockCommand, Unit, State] with {

        override def runState[M[_]: MonadThrow](
            cmd: StartBlockCommand
        )(using log: ContraTracer[M, Slf4jMsg]): StateT[M, State, Unit] =
            StateT.modifyF[M, State] { state =>
                state.getCurrentTime match {
                    case CurrentTime.BeforeHappyPathExpiration(_) =>
                        val newBlockM = state.blockCycle match {
                            case BlockCycle.Ready(prevBlockNumber, prevVersion)
                                if prevBlockNumber.increment == cmd.blockNumber =>
                                MonadThrow[M].pure(
                                  BlockCycle.InProgress(
                                    blockNumber = cmd.blockNumber,
                                    blockStartTime =
                                        BlockCreationStartTime(state.getCurrentTime.instant),
                                    prevVersion = prevVersion
                                  )
                                )
                            case _ =>
                                MonadThrow[M].raiseError(
                                  UnexpectedState("StartBlockCommand requires BlockCycle.Ready")
                                )
                        }
                        for
                            newBlock <- newBlockM
                            _ <- log.debug(
                              s"MODEL>> StartBlockCommand for block number: ${cmd.blockNumber}"
                            )
                        yield state.copy(blockCycle = newBlock)
                    case _ =>
                        MonadThrow[M].raiseError(
                          Error.UnexpectedState(
                            "StartBlockCommand requires CurrentTime.BeforeHappyPathExpiration"
                          )
                        )
                }
            }
    }

    // ===================================
    // CompleteBlockCommand
    // ===================================

    given ModelCommand[CompleteBlockCommand, BlockBrief, State] with {

        override def runState[M[_]: MonadThrow](
            cmd: CompleteBlockCommand
        )(using log: ContraTracer[M, Slf4jMsg]): StateT[M, State, BlockBrief] =
            for
                state <- StateT.get[M, State]
                brief <- state.blockCycle match {
                    case BlockCycle.InProgress(blockNumber, _creationTime, prevVersion, accumulator) =>
                        val events: List[(RequestId, ValidityFlag)] =
                            accumulator.map((le, _, flag) => le.requestId -> flag)
                        for
                            _                   <- StateT.liftF(
                                                     log.debug(
                                                       s"MODEL>> CompleteBlockCommand for block number: ${cmd.blockNumber}"
                                                     )
                                                   )
                            regOrReject         <- registerOrReject[M](events)
                            registeredThisBlock  = regOrReject._1
                            rejectedThisBlock    = regOrReject._2
                            absorbedThisBlock   <- absorb[M](cmd.blockCreationEndTime)
                            refundedThisBlock   <- refund[M](cmd.isFinal, cmd.blockCreationEndTime)
                            blockBrief          <- mkBlockBrief[M](
                                                     cmd.isFinal,
                                                     cmd.blockCreationEndTime,
                                                     cmd.blockNumber,
                                                     prevVersion,
                                                     accumulator,
                                                     absorbedThisBlock,
                                                     refundedThisBlock
                                                   )
                            // tick time
                            _                   <- StateT.modify[M, State](
                                                     _.advanceCurrentTime(cmd.blockCreationEndTime.convert)
                                                   )
                            // update block cycle
                            _                   <- StateT.modify[M, State] { state =>
                                                     val nextBlockCycle =
                                                         if cmd.isFinal then BlockCycle.HeadFinalized
                                                         else BlockCycle.Done(cmd.blockNumber, blockBrief.blockVersion)
                                                     state.copy(blockCycle = nextBlockCycle)
                                                   }
                        yield blockBrief
                    case _ =>
                        StateT.liftF(
                          MonadThrow[M].raiseError(
                            UnexpectedState("CompleteBlockCommand requires BlockCycle.InProgress")
                          )
                        )
                }
            yield brief

        private def getBlockCreationStartTime[M[_]: Applicative]: StateT[M, State, BlockCreationStartTime] =
            StateT.inspect(state => BlockCreationStartTime(state.getCurrentTime.instant))

        private def getNewSettlementValidityEnd[M[_]: Applicative]: StateT[M, State, SettlementTxEndTime] =
            StateT.inspect(state =>
                state.multiNodeConfig.txTiming.newSettlementEndTime(state.competingFallbackStartTime)
            )

        /** Register or reject [[Enqueued]] deposits depending on their [[ValidityFlag]], as derived
          * from the [[BlockAccumulator]].
          * @return
          *   a tuple of (registeredEvents, rejectedEvents)
          */
        private def registerOrReject[M[_]: Monad](
            events: List[(RequestId, ValidityFlag)]
        ): StateT[M, State, (Queue[Registered], Queue[Rejected])] = for {
            state <- StateT.get[M, State]
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
                        thisEvent.map((_, validityFlag) => (validityFlag, cmd))
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
            registered <- liftS(DepositStatus.Registered.register(depositsToRegisterOrReject._1))
            rejected <- liftS(DepositStatus.Rejected.reject(depositsToRegisterOrReject._2))
        } yield (registered, rejected)

        def absorb[M[_]: Applicative](
            blockCreationEndTime: BlockCreationEndTime
        ): StateT[M, State, Queue[Absorbed]] =
            // The fast cycle does not rotate the treasury, so no deposit ever transitions to
            // Absorbed. Mature Submitted deposits flow to `refund` instead (see SUT's
            // `NotInPollResults` compartment in `DepositsMap.partition`).
            StateT.pure(Queue.empty)

        // Previous absorb body (kept for review; restore once the slow cycle rotates the
        // treasury and absorption can actually happen on the fast/slow split):
        //
        // def absorb(
        //     blockCreationEndTime: BlockCreationEndTime
        // ): cats.data.State[State, Queue[Absorbed]] = for {
        //     state <- cats.data.State.get[State]
        //     settlementValidityEnd <- getNewSettlementValidityEnd
        //
        //     depositsToAbsorb: Queue[Submitted] = {
        //         given TxTiming.Section = state.multiNodeConfig
        //         val eligible = state.deposits.depositsSubmitted
        //             .filter { submitted =>
        //                 {
        //
        //                     logger.trace(
        //                       s"MODEL deposit absorption check: ${submitted.request.requestId},\n" +
        //                           s"depositAbsorptionStart=${submitted.depositAbsorptionStart}, " +
        //                           s"depositAbsorptionEnd=${submitted.depositAbsorptionEnd}"
        //                     )
        //
        //                     // Check all the conditions
        //                     // mature
        //                     submitted.depositAbsorptionStart.convert <= blockCreationEndTime
        //                     // Fits in validity window
        //                     && submitted.depositAbsorptionEnd.convert >= settlementValidityEnd.convert
        //                 }
        //             }
        //         val byStartTime = eligible.sortBy(_.depositAbsorptionStart)
        //         byStartTime.take(state.multiNodeConfig.headConfig.maxDepositsAbsorbedPerBlock)
        //     }
        //
        //     _ = logger.trace(s"depositsToAbsorb: $depositsToAbsorb")
        //
        //     depositsAbsorbed <- DepositStatus.Absorbed.absorb(depositsToAbsorb)
        // } yield depositsAbsorbed

        private def refund[M[_]: Monad](
            isFinal: Boolean,
            blockCreationEndTime: BlockCreationEndTime
        ): StateT[M, State, Queue[Refunded]] = for {
            state <- StateT.get[M, State]
            settlementValidityEnd <- getNewSettlementValidityEnd[M]

            depositsToRefund: Queue[Refundable] =
                if isFinal
                then
                    state.deposits.hydrozoaKnownRegisteredDeposits // all known deposits should be refunded
                else
                    state.deposits.hydrozoaKnownRegisteredDeposits.filter(refundable =>
                        given TxTiming.Section = state.multiNodeConfig

                        // On the fast-only path nothing is ever absorbed, so any mature deposit
                        // (Submitted included) refunds as soon as the SUT classifies it
                        // `NotInPollResults`. The expired clause stays as a belt-and-braces.
                        refundable.depositAbsorptionEnd.convert < settlementValidityEnd.convert
                        || refundable.depositAbsorptionStart.convert <= blockCreationEndTime
                    )
                    // Previous predicate (restore when the slow cycle wires absorption back in):
                    //
                    // refundable.depositAbsorptionEnd.convert < settlementValidityEnd.convert
                    // || (refundable.depositAbsorptionStart.convert <= blockCreationEndTime && !refundable
                    //     .isInstanceOf[Submitted])
            refunded <- liftS(DepositStatus.Refunded.refund(depositsToRefund))
        } yield refunded

        private def majorBlock[M[_]: Monad](
            blockEndTime: BlockCreationEndTime,
            blockNumber: BlockNumber,
            blockVersion: BlockVersion.Full,
            events: List[(RequestId, ValidityFlag)],
            absorbedThisBlock: Queue[Absorbed],
            refundedThisBlock: Queue[Refunded]
        ): StateT[M, State, BlockBrief.Major] =
            for {
                state <- StateT.get[M, State]
                txTiming = state.multiNodeConfig.txTiming

                blockStartTime <- getBlockCreationStartTime[M]

                newFallbackTxStartTime = txTiming.newFallbackStartTime(blockEndTime)
                newForcedMajorBlockWakeupTime =
                    txTiming.forcedMajorBlockWakeupTime(newFallbackTxStartTime)
                mAbsorptionStartTime = state.mAbsorptionStartTime
                newDepositDecisionWakeupTime =
                    mAbsorptionStartTime.map(t => DepositDecisionWakeupTime(t.convert))

                majorBlock = Major(
                  header = BlockHeader.Major(
                    blockNum = blockNumber,
                    blockVersion = blockVersion,
                    startTime = blockStartTime,
                    endTime = blockEndTime,
                    fallbackTxStartTime = newFallbackTxStartTime,
                    forcedMajorBlockWakeupTime = newForcedMajorBlockWakeupTime,
                    mDepositDecisionWakeupTime = newDepositDecisionWakeupTime
                  ),
                  body = BlockBody.Major(
                    events = events,
                    depositsAbsorbed = absorbedThisBlock.map(_.cmd.request.requestId).toList,
                    depositsRefunded = refundedThisBlock.map(_.cmd.request.requestId).toList
                  )
                )

                _ <- StateT.modify[M, State](
                  _.copy(competingFallbackStartTime = newFallbackTxStartTime)
                )

            } yield majorBlock

        private def minorBlock[M[_]: Monad](
            blockEndTime: BlockCreationEndTime,
            blockNumber: BlockNumber,
            blockVersion: BlockVersion.Full,
            events: List[(RequestId, ValidityFlag)],
            refundedThisBlock: Queue[Refunded]
        ): StateT[M, State, BlockBrief.Minor] = for {
            state <- StateT.get[M, State]
            blockStartTime <- getBlockCreationStartTime[M]
            forcedMajorBlockWakeupTime =
                state.multiNodeConfig.txTiming.forcedMajorBlockWakeupTime(
                  state.competingFallbackStartTime
                )
            mAbsorptionStartTime = state.mAbsorptionStartTime
            mDepositDecisionWakeupTime =
                mAbsorptionStartTime.map(t => DepositDecisionWakeupTime(t.convert))
            minorBlock = Minor(
              header = BlockHeader.Minor(
                blockNum = blockNumber,
                blockVersion = blockVersion,
                startTime = blockStartTime,
                endTime = blockEndTime,
                fallbackTxStartTime = state.competingFallbackStartTime,
                forcedMajorBlockWakeupTime = forcedMajorBlockWakeupTime,
                mDepositDecisionWakeupTime = mDepositDecisionWakeupTime
              ),
              body = BlockBody.Minor(
                events = events,
                depositsRefunded = refundedThisBlock.map(_.cmd.request.requestId).toList
              )
            )
        } yield minorBlock

        def finalBlock[M[_]: Monad](
            blockEndTime: BlockCreationEndTime,
            blockNumber: BlockNumber,
            blockVersion: BlockVersion.Full,
            events: List[(RequestId, ValidityFlag)],
            refundedThisBlock: Queue[Refunded]
        ): StateT[M, State, BlockBrief.Final] = for {
            _ <- StateT.modify[M, State](_.copy(blockCycle = BlockCycle.HeadFinalized))
            blockStartTime <- getBlockCreationStartTime[M]

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

        private def mkBlockBrief[M[_]: Monad](
            isFinal: Boolean,
            blockEndTime: BlockCreationEndTime,
            blockNumber: BlockNumber,
            prevVersion: BlockVersion.Full,
            accumulator: BlockAccumulator,
            absorbedThisBlock: Queue[Absorbed],
            refundedThisBlock: Queue[Refunded]
        ): StateT[M, State, BlockBrief] =
            for {
                state <- StateT.get[M, State]

                events = accumulator.map((req, _, flag) => (req.requestId, flag))

                // Construct, but don't execute the state transitions -- we decide which one we need below
                doMajorBlock = majorBlock[M](
                  blockEndTime,
                  blockNumber,
                  prevVersion.incrementMajor,
                  events,
                  absorbedThisBlock,
                  refundedThisBlock
                )
                doMinorBlock = minorBlock[M](
                  blockEndTime,
                  blockNumber,
                  prevVersion.incrementMinor,
                  events,
                  refundedThisBlock
                )
                doFinalBlock = finalBlock[M](
                  blockEndTime,
                  blockNumber,
                  prevVersion.incrementMajor,
                  events,
                  refundedThisBlock
                )

                blockCanStayMinor: Boolean = state.multiNodeConfig.txTiming
                    .blockCanStayMinor(blockEndTime, state.competingFallbackStartTime)

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

            } yield brief

        override def preCondition(cmd: CompleteBlockCommand, state: State): Boolean =
            state.blockCycle match {
                case BlockCycle.InProgress(currentBlockNumber, _, _, _) =>
                    cmd.blockNumber == currentBlockNumber
                case _ => false
            }

        override def delay(cmd: CompleteBlockCommand): FiniteDuration =
            cmd.blockDuration.finiteDuration
    }

    // ===================================
    // L2TxCommand
    // ===================================

    given ModelCommand[L2TxCommand, Unit, State] with {

        override def runState[M[_]: MonadThrow](
            cmd: L2TxCommand
        )(using log: ContraTracer[M, Slf4jMsg]): StateT[M, State, Unit] =
            StateT.modifyF[M, State] { state =>
                for
                    l2Tx <- L2Tx
                        .parse(
                          cmd.request.request.body.l2Payload.bytes,
                          state.multiNodeConfig.cardanoNetwork
                        )
                        .fold(
                          err =>
                              MonadThrow[M].raiseError(
                                RuntimeException(s"Failed to parse L2Tx: $err")
                              ),
                          MonadThrow[M].pure(_)
                        )
                    ret = HydrozoaTransactionMutator.transit(
                      config = state.multiNodeConfig.headConfig,
                      time = state.getCurrentTime.instant,
                      state = state.utxosL2Active,
                      l2Tx = l2Tx
                    )
                    (newState, mInvalidMsg) = ret match {
                        case Left(err) =>
                            (
                              blockCycleLens
                                  .andThen(contentLens)
                                  .modify(_ :+ (cmd.request, l2Tx, ValidityFlag.Invalid))(state),
                              Some(s"invalid L2 tx ${cmd.request.requestId}: $err")
                            )
                        case Right(mutatorState) =>
                            (
                              state
                                  .pipe(
                                    blockCycleLens
                                        .andThen(contentLens)
                                        .modify(_ :+ (cmd.request, l2Tx, ValidityFlag.Valid))
                                  )
                                  .focus(_.utxosL2Active)
                                  .replace(mutatorState),
                              None
                            )
                    }
                    finalState = newState.focus(_.nextRequestNumber).modify(_.increment)
                    _ <- log.debug(s"MODEL>> L2TxCommand for event ID: ${cmd.request.requestId}")
                    _ <- mInvalidMsg.fold(MonadThrow[M].pure(()))(log.debug(_))
                yield finalState
            }
    }

    // ===================================
    // RegisterDepositCommand
    // ===================================

    given ModelCommand[RegisterDepositCommand, Unit, State] with {
        override def runState[M[_]: MonadThrow](
            cmd: RegisterDepositCommand
        )(using log: ContraTracer[M, Slf4jMsg]): StateT[M, State, Unit] =
            StateT.modifyF[M, State] { state =>
                import cmd.request as req
                import state.multiNodeConfig as config

                val BlockCycle.InProgress(_, blockStartTime, _, _) = state.blockCycle: @unchecked

                val requestValidityEndTime = req.request.header.validityEnd

                for
                    seq <- DepositRefundTxSeq
                        .Parse(config.headConfig)(
                          depositTxBytes = req.request.body.l1Payload,
                          l2Payload = req.request.body.l2Payload,
                          requestId = req.requestId,
                          requestValidityEndTime = requestValidityEndTime
                        )
                        .result
                        .fold(
                          e => MonadThrow[M].raiseError(RuntimeException(e)),
                          MonadThrow[M].pure(_)
                        )
                    depositAbsorptionEndTime =
                        config.headConfig.txTiming.depositAbsorptionEndTime(requestValidityEndTime)
                    // For now, all deposits request should be valid by construction
                    _ <- MonadThrow[M].raiseUnless(
                           blockStartTime < seq.depositTx.submissionDeadline
                         )(RuntimeException("deposit past submissionDeadline"))
                    _ <- MonadThrow[M].raiseUnless(blockStartTime < depositAbsorptionEndTime)(
                           RuntimeException("deposit past absorptionEndTime")
                         )
                    depositUtxo = seq.depositTx.depositProduced
                    newState = state
                        .pipe(
                          blockCycleLens
                              .andThen(contentLens)
                              .modify(_ :+ (cmd.request, depositUtxo, ValidityFlag.Valid))
                        )
                        .pipe(DepositStatus.Enqueued.enqueue(Queue(cmd)))
                        .focus(_.utxoLocked)
                        .modify(_ ++ seq.depositTx.tx.body.value.inputs.toSeq)
                        .focus(_.nextRequestNumber)
                        .modify(_.increment)
                    _ <- log.debug(
                      s"MODEL>> RegisterDepositCommand for event ID: ${cmd.request.requestId}"
                    )
                yield newState
            }
    }

    // ===================================
    // SubmitDepositsCommand
    // ===================================

    given ModelCommand[SubmitDepositsCommand, Unit, State] with {
        override def runState[M[_]: MonadThrow](
            cmd: SubmitDepositsCommand
        )(using log: ContraTracer[M, Slf4jMsg]): StateT[M, State, Unit] =
            for
                _ <- liftS(DepositStatus.Submitted.submit(cmd.depositsForSubmission))
                _ <- liftS(DepositStatus.Declined.decline(cmd.depositsToDecline))
                _ <- StateT.liftF(
                       log.debug(
                         s"MODEL>> SubmitDepositCommand, for submission: ${cmd.depositsForSubmission.size}, " +
                             s"for rejection: ${cmd.depositsToDecline.size}"
                       )
                     )
            yield ()
    }

    enum Error extends Throwable:
        case UnexpectedState(msg: String)

        override def getMessage: String = this match {
            case Error.UnexpectedState(msg) => s"Unexpected state while stepping the model: $msg"
        }

end Model
