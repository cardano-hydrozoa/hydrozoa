package hydrozoa.integration.stage1

import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.stage1.Commands.*
import hydrozoa.integration.stage1.Model.Error.UnexpectedState
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.given_Ordering_QuantizedInstant.mkOrderingOps
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockBrief.{Final, Major, Minor}
import hydrozoa.multisig.ledger.block.{BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.eutxol2.tx.L2Genesis.mkGenesisId
import hydrozoa.multisig.ledger.eutxol2.tx.{GenesisObligation, L2Genesis, L2Tx, genesisObligationDecoder}
import hydrozoa.multisig.ledger.eutxol2.{HydrozoaTransactionMutator, toEvacuationKey}
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag.Valid
import hydrozoa.multisig.ledger.event.RequestNumber.increment
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.joint.given
import hydrozoa.multisig.ledger.joint.{EvacuationKey, EvacuationMap}
import hydrozoa.multisig.ledger.l1.txseq.DepositRefundTxSeq
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import hydrozoa.multisig.server.UserRequest
import io.bullet.borer.Cbor
import monocle.Lens
import monocle.syntax.all.focus
import org.scalacheck.commands.ModelCommand

import scala.collection.immutable.{Queue, TreeMap}
import scala.util.chaining.*
import scalus.cardano.ledger.{AssetName, KeepRaw, Transaction, TransactionHash, TransactionInput, TransactionOutput, Utxos}

object Model:
    private val logger = Logging.logger("Stage1.Model")

    // ===================================
    // Model state
    // ===================================

    /** This should be immutable. Only contains what's needed for model operations and SUT
      * construction.
      */
    case class State(
        // Non-mutable
        multiNodeConfig: MultiNodeConfig,
        // Initial state of the peer's L1 utxos.
        // It's needed since [[peerUtxosL1]] reflects the state after running the initialization tx
        // which applies upon initial state generation (is there a better spot to run the init tx?)
        peerGenesisUtxosL1: Utxos,

        // "Mutable" part
        nextRequestNumber: RequestNumber,
        currentTime: CurrentTime,

        // Block producing cycle
        blockCycle: BlockCycle,

        // This is put here to avoid tossing over Done/Ready/InProgress
        // NB: for block zero it's more initializationExpirationTime
        competingFallbackStartTime: QuantizedInstant,

        // Evacuation Map
        evacuationMap: EvacuationMap,
        // L1 state - the only peer's utxos
        peerUtxosL1: Utxos,

        // Deposits

        // The queue of all generated deposits that Alice intends to register.
        // At all times, all deposits in the list are disjoint in terms of their funding utxo.
        depositEnqueued: List[RegisterDepositCommand],
        // Signed deposit transactions - we need them when we submit deposits.
        depositSigned: Map[TransactionHash, Transaction],
        // Utxos used in the deposit enqueued as funding utxos.
        // We need this not to generate deposits that use the same utxos for funding many times.
        utxoLocked: Set[TransactionInput],

        // Subset of depositEnqueued which has been registered by Hydrozoa, i.e.
        // included in a block brief with positive validity flag.
        depositsRegistered: List[RequestId],

        // After a deposit was registered, we may submit it or cancel it depending on
        // how much time is left until its deposit tx's TTL is up - I call it runway.
        // Upon generating [[SubmitDepositsCommand]] we assess whether we have enough
        // runway to take off the deposit - i.e. how much time we have from now to the
        // ttl. This is needed, because the test fails if SUT can't submit deposit tx
        // that model expects to see.
        //
        // So we have two partitions here:
        //  - deposits, that have been submitted, so they are expected to appear in the
        // very first block that satisfies their absorption window
        //  - deposits, that the model decided not to submit - their funding utdxos get
        // unlocked so they can be used again
        depositSubmitted: List[RequestId],
        depositRejected: List[RequestId],
    ) {
        override def toString: String = "<model state (hidden)>"

        def nextRequestId: RequestId =
            RequestId(peerNum = HeadPeerNumber.zero, requestNum = nextRequestNumber)

        /** To save time and keep things simple we exploit the fact that all txs that may mutate the
          * L1 state of the peer's utxo are continuing - they spend and pays back at least one utxo
          * that belongs to the peer. So we can always calculate peer's addresses using the
          * preexisting state.
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
                    cmd.registerDeposit.requestId -> cmd.depositRefundTxSeq.depositTx.submissionDeadline
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
                  UserRequest[_],
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
        : Lens[BlockCycle.InProgress, List[(UserRequest[_], L2Tx | DepositUtxo, ValidityFlag)]] =
        Lens[BlockCycle.InProgress, List[(UserRequest[_], L2Tx | DepositUtxo, ValidityFlag)]](
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

            logger.debug(s"MODEL>> L2TxCommand for event ID: ${cmd.event.requestId}")

            val BlockCycle.InProgress(_, _, _, currentEvents) = state.blockCycle: @unchecked
            logger.trace(s"INPUT state.blockCycle event IDs: ${currentEvents.map(_._1.requestId)}")

            val l2Tx: L2Tx = L2Tx
                .parse(cmd.event.l2Payload)
                .fold(err => throw RuntimeException(s"Failed to parse L2Tx: $err"), identity)

            val ret = HydrozoaTransactionMutator.transit(
              config = state.multiNodeConfig.headConfig,
              time = state.currentTime.instant,
              state = state.evacuationMap.cooked.map((ek, o) =>
                  Cbor.decode(ek.byteString).to[TransactionInput].value -> o
              ),
              l2Tx = l2Tx
            )

            val newState = ret match {
                case Left(err) =>
                    logger.debug(s"invalid L2 tx ${cmd.event.requestId}: ${err}")
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
                        .replace(
                          EvacuationMap(
                            TreeMap.from(
                              mutatorState.map((ti, to) => ti.toEvacuationKey -> KeepRaw(to))
                            )
                          )
                        )
            }

            val finalState = newState
                .focus(_.nextLedgerEventNumber)
                .modify(_.increment)

            val BlockCycle.InProgress(_, _, _, finalEvents) = finalState.blockCycle: @unchecked
            logger.trace(
              s"OUTPUT finalState.blockCycle event IDs: ${finalEvents.map(_._1.requestId)}"
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
                      s"Completing block with ${events.length} events: ${events.map(_._1.body.requestId)}"
                    )
                    val (blockBrief, newActiveUtxos, withdrawnUtxos) = mkBlockBrief(
                      cmd.blockNumber,
                      events,
                      state.competingFallbackStartTime,
                      state.multiNodeConfig.headConfig.txTiming,
                      creationTime,
                      prevVersion,
                      cmd.isFinal,
                      state.evacuationMap,
                      state.depositEnqueued,
                      state.depositsRegistered,
                      state.depositSubmitted,
                      state.multiNodeConfig.headConfig.headTokenNames.treasuryTokenName
                    )

                    val newCompetingFallbackStartTime =
                        if blockBrief.isInstanceOf[Major]
                        then
                            state.multiNodeConfig.headConfig.txTiming
                                .newFallbackStartTime(creationTime)
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
                            cmd._1.requestId
                          )
                      ),
                      depositsRegistered = state.depositsRegistered
                          ++ blockBrief.events
                              .filter(_._2 == Valid)
                              .map(_._1)
                              .filter(e =>
                                  state.depositEnqueued.map(_.registerDeposit.requestId).contains(e)
                              ),
                      evacuationMap = newActiveUtxos,
                      peerUtxosL1 = state.peerUtxosL1 // ++  withdrawnUtxos.cooked
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
            events: List[(UserRequest, L2Tx | DepositUtxo, ValidityFlag)],
            competingFallbackStartTime: QuantizedInstant,
            txTiming: TxTiming,
            blockStartTime: QuantizedInstant,
            prevVersion: BlockVersion.Full,
            isFinal: Boolean,
            evacuationMap: EvacuationMap,
            depositEnqueued: List[RegisterDepositCommand],
            depositRegistered: List[RequestId],
            depositsSubmitted: List[RequestId],
            treasuryTokenName: AssetName
        ): (BlockBrief, EvacuationMap, EvacuationMap) = {

            logger.trace(s"mkBlockBrief: blockNumber: $blockNumber")
            logger.trace(s"mkBlockBrief: blockStartTime: $blockStartTime")
            val settlementValidityEnd =
                txTiming.newSettlementEndTime(competingFallbackStartTime)
            logger.trace(s"settlementValidityEnd=$settlementValidityEnd")

            val events_ = events.map((le, _, flag) => le.requestId -> flag)

            val depositsToCheck =
                depositEnqueued.filter(d => depositRegistered.contains(d.registerDeposit.requestId))

            val depositsAbsorbed = depositsToCheck
                .filter(cmd => {
                    val submissionDeadline = cmd.depositRefundTxSeq.depositTx.submissionDeadline
                    val depositAbsorptionStart =
                        txTiming.depositAbsorptionStartTime(submissionDeadline)
                    val depositAbsorptionEnd = txTiming.depositAbsorptionEndTime(submissionDeadline)
                    val depositUtxoFound = depositsSubmitted.contains(cmd.registerDeposit.requestId)

                    logger.trace(
                      s"MODEL deposit eligibility check: ${cmd.registerDeposit.requestId},\n" +
                          s"depositAbsorptionStart=$depositAbsorptionStart, " +
                          s"depositAbsorptionEnd=$depositAbsorptionEnd, " +
                          s"utxo is found: $depositUtxoFound"
                    )

                    depositAbsorptionStart <= blockStartTime
                    && depositAbsorptionEnd >= settlementValidityEnd
                    && depositUtxoFound
                })
                .map(_._1.requestId)

            logger.trace(s"depositsAbsorbed: $depositsAbsorbed")

            val depositsRefunded =
                (if isFinal
                 then depositsToCheck // all known deposits should be refunded
                 else
                     depositsToCheck.filter(cmd =>
                         val submissionDeadline = cmd.depositRefundTxSeq.depositTx.submissionDeadline
                         val depositAbsorptionStart =
                             txTiming.depositAbsorptionStartTime(submissionDeadline)
                         val settlementValidityEnd =
                             txTiming.newSettlementEndTime(competingFallbackStartTime)
                         val depositAbsorptionEnd =
                             txTiming.depositAbsorptionEndTime(submissionDeadline)
                         val depositUtxoFound =
                             depositsSubmitted.contains(cmd.registerDeposit.requestId)

                         logger.trace(
                           s"MODEL deposit rejection check: ${cmd.registerDeposit.requestId},\n" +
                               s"depositAbsorptionEnd=$depositAbsorptionEnd, " +
                               s"settlementValidityEnd=$settlementValidityEnd"
                         )

                         settlementValidityEnd > depositAbsorptionEnd ||
                         (depositAbsorptionStart < blockStartTime && !depositUtxoFound)
                     )
                ).map(_._1.requestId)

            logger.trace(s"depositsRefunded: $depositsRefunded")

            val genesisObligations: List[GenesisObligation] =
                depositsAbsorbed
                    .map(eventId =>
                        depositEnqueued
                            .find(_.registerDeposit.requestId == eventId)
                            .getOrElse(
                              throw RuntimeException(s"deposit not found: $eventId")
                            )
                    )
                    .flatMap(rdc => {
                        val l2Payload: Array[Byte] =
                            rdc.depositRefundTxSeq.depositTx.depositProduced.l2Payload
                        Cbor.decode(l2Payload).to[Queue[GenesisObligation]].value.toList
                    })

            val genesisUtxos: Option[TreeMap[EvacuationKey, KeepRaw[TransactionOutput]]] = for {
                obligations <-
                    if genesisObligations.nonEmpty
                    then Some(genesisObligations)
                    else None
                genesisId = mkGenesisId(???)
                l2Genesis = L2Genesis(Queue.from(obligations), genesisId)
            } yield l2Genesis.asUtxos.map((ti, krto) => ti.toEvacuationKey -> krto)

            val newEvacuationMap = evacuationMap.appended(
              genesisUtxos.getOrElse(TreeMap.empty[EvacuationKey, KeepRaw[TransactionOutput]])
            )

            // TODO: the idea was to reuse withdrawn utxos on L1.
            // The problem with that approach is that the model knows nothing about the effects;
            // So we don't know utxo ids in the model, we cannot reuse them.
            val withdrawnUtxos: EvacuationMap = EvacuationMap.empty

            lazy val majorBlock = Major(
              header = BlockHeader.Major(
                blockNum = blockNumber,
                blockVersion = prevVersion.incrementMajor,
                startTime = blockStartTime,
                kzgCommitment = newEvacuationMap.kzgCommitment
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
                events = events.map((le, _, flag) => le.requestId -> flag),
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
                events = events.map((le, _, flag) => le.requestId -> flag),
                depositsRefunded = depositsRefunded
              )
            )

            val brief =
                if isFinal then finalBlock
                else if txTiming.blockCanStayMinor(blockStartTime, competingFallbackStartTime) // FIXME: blockCreationEndTime
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

            (brief, newEvacuationMap, withdrawnUtxos)
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
            import state.multiNodeConfig as config

            logger.debug(
              s"MODEL>> RegisterDepositCommand for event ID: ${cmd.registerDeposit.requestId}"
            )

            val BlockCycle.InProgress(_, blockStartTime, _, currentEvents) =
                state.blockCycle: @unchecked
            logger.trace(s"INPUT state.blockCycle event IDs: ${currentEvents.map(_._1.requestId)}")

            val seq =
                DepositRefundTxSeq
                    .Parse(config.headConfig)(
                      depositTxBytes = req.depositTxBytes,
                      refundTxBytes = req.refundTxBytes,
                      l2Payload = req.l2Payload
                    )
                    .result
                    .fold(e => throw RuntimeException(e), identity)

            // For now, all deposits request should be valid by construction
            require(blockStartTime < seq.depositTx.submissionDeadline)
            require(
              blockStartTime < config.headConfig.txTiming.depositAbsorptionEndTime(
                seq.depositTx.submissionDeadline
              )
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
                .focus(_.nextRequestNumber)
                .modify(_.increment)

            val BlockCycle.InProgress(_, _, _, finalEvents) = finalState.blockCycle: @unchecked

            logger.trace(
              s"OUTPUT finalState.blockCycle event IDs: ${finalEvents.map(_._1.requestId)}"
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
              s"MODEL>> SubmitDepositCommand, for submission: ${cmd.depositsForSubmission.size}, " +
                  s"for rejection: ${cmd.depositsForRejection.size}"
            )

            val allDeposits = cmd.depositsForSubmission.map(e => true -> e._1)
                ++ cmd.depositsForRejection.map(e => false -> e)

            // Attach corresponding register deposit commands
            val depositsAug = allDeposits.map((flag, eventId) =>
                val registerDepositCmd = state.depositEnqueued
                    .find(_.registerDeposit.requestId == eventId)
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
