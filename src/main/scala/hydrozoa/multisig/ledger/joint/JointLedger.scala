package hydrozoa.multisig.ledger.joint

import cats.effect.{IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastOps
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.lib.actor.*
import hydrozoa.lib.cardano.scalus.QuantizedTime
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.{ConsensusActor, PeerLiaison}
import hydrozoa.multisig.ledger.block.*
import hydrozoa.multisig.ledger.event.LedgerEventId.ValidityFlag
import hydrozoa.multisig.ledger.event.LedgerEventId.ValidityFlag.{Invalid, Valid}
import hydrozoa.multisig.ledger.event.UserEvent.*
import hydrozoa.multisig.ledger.event.{LedgerEventId, UserEvent}
import hydrozoa.multisig.ledger.joint.EvacuationMap.applyDiffs
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l1.L1LedgerM
import hydrozoa.multisig.ledger.l1.L1LedgerM.*
import hydrozoa.multisig.ledger.l1.tx.RefundTx
import hydrozoa.multisig.ledger.l1.txseq.{FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import hydrozoa.multisig.ledger.l2.{L2Ledger, L2LedgerError, L2LedgerEvent, L2LedgerState}
import monocle.Focus.focus
import scala.collection.immutable.Queue
import scala.math.Ordered.orderingToOrdered
import scalus.cardano.ledger.TransactionInput

import JointLedger.*
import JointLedger.Requests.*

// Fields of a work-in-progress block pertaining to user events, with an additional field for dealing with withdrawn utxos
private case class UserEventState(
    events: List[(LedgerEventId, ValidityFlag)],
    postDatedRefundTxs: Vector[RefundTx.PostDated]
)

// NOTE: Joint ledger is created by the MultisigManager.
// NOTE: As of 2025-11-16, George says BlockWeaver should be the ONLY actor calling the joint ledger
final case class JointLedger(
    config: JointLedger.Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | JointLedger.Connections,
    l2Ledger: L2Ledger[IO]
) extends Actor[IO, Requests.Request] {
    import config.*

    private val logger = Logging.logger("JointLedger")

    private val connections = Ref.unsafe[IO, Option[Connections]](None)

    val state: Ref[IO, JointLedger.State] =
        Ref.unsafe[IO, JointLedger.State](
          Done(
            producedBlock = config.initialBlock,
            lastFallbackValidityStart = config.initialFallbackTx.validityStart,
            l1LedgerState =
                L1LedgerM.State(config.initializationTx.treasuryProduced, DepositsMap.empty),
            evacuationMap = config.initialEvacuationMap
          )
        )

    private def getConnections: IO[Connections] = for {
        mConn <- this.connections.get
        conn <- mConn.fold(
          IO.raiseError(
            java.lang.Error(
              "Joint ledger is missing its connections to other actors."
            )
          )
        )(IO.pure)
    } yield conn

    private def initializeConnections: IO[Unit] = pendingConnections match {
        case x: MultisigRegimeManager.PendingConnections =>
            for {
                _connections <- x.get
                _ <- connections.set(
                  Some(
                    Connections(
                      consensusActor = _connections.consensusActor,
                      peerLiaisons = _connections.peerLiaisons
                    )
                  )
                )
            } yield ()
        case x: JointLedger.Connections => connections.set(Some(x))
    }

    // TODO: Refactor to use "become" and use different receive functions

    /** Get _only_ a [[Producing]] State or throw an exception QUESTION: What type of exception
      * should this be?
      */
    private val unsafeGetProducing: IO[Producing] = for {
        s <- state.get
        p <- s match {
            case _: Done =>
                throw new RuntimeException(
                  "Expected a `Producing` State, but got `Done`. This indicates" +
                      " that a request was issued to the JointLedger that is only valid when the hydrozoa node is producing" +
                      " a block."
                )

            case p: Producing => IO.pure(p)
        }
    } yield p

    /** Get _only_ a [[Done]] State or throw an exception QUESTION: What type of exception should
      * this be?
      */
    private val unsafeGetDone: IO[Done] = for {
        s <- state.get
        p <- s match {
            case _: Producing =>
                throw new RuntimeException(
                  "Expected a `Done` State, but got `Producing`. This indicates" +
                      " that a request was issued to the JointLedger that is only valid when the hydrozoa node is not producing" +
                      " a block."
                )
            case d: Done => IO.pure(d)
        }
    } yield p

    override def preStart: IO[Unit] = initializeConnections

    // TODO: PartialFunction.fromFunction is a noop here
    override def receive: Receive[IO, Requests.Request] = PartialFunction.fromFunction {
        case e: UserEvent            => applyUserEvent(e)
        case s: StartBlock           => startBlock(s)
        case c: CompleteBlockRegular => completeBlockRegular(c)
        case f: CompleteBlockFinal   => completeBlockFinal(f)
        case req: SyncRequest.Any =>
            req.request match {
                case r: GetState.type => r.handleSync(req, _ => state.get)
            }
    }

    /** Run an [[l2ledger.L2LedgerAction]] within a JointLedger. If the action is successful
      * (returns `Right`), the state of the JointLedger is updated. Because the state update within
      * JointLedger must happen within [[IO]], this takes two continuations (one for success, one
      * for failure) and returns in [[IO]].
      *
      * @param onFailure
      *   continuation if an error is raised. Defaults to throwing an exception.
      * @param onSuccess
      *   continuation if a value is returned. Defaults to IO.pure
      * @return
      */
    private def runL2LedgerAction[A, B](
        action: l2Ledger.L2LedgerAction,
        onFailure: L2LedgerError => IO[B] = e =>
            // FIXME: Type the exception better
            throw new RuntimeException(s"Error running L2LedgerAction: $e"),
        onSuccess: L2LedgerState => IO[B] = ls => IO.pure(ls)
    ): IO[B] = {
        for {
            oldState <- unsafeGetProducing
            res <- action.run(
              oldState.l2LedgerState
            )
            b <- res match {
                case Left(error) => onFailure(error)
                case Right(newState) =>
                    for {
                        // WARNING: This is _effectful_. If the `onSuccess` action throws an exception, this
                        // state update will still take effect.
                        _ <- this.state.set(oldState.focus(_.l2LedgerState).replace(newState))
                        b <- onSuccess(newState)
                    } yield b
            }
        } yield b
    }

    private def applyUserEvent(e: UserEvent): IO[Unit] = {
        e match {
            case req: DepositEvent => registerUserDeposit(req)
            case tx: L2Event       => applyL2UserEvent(tx)
        }
    }

    /** Update the JointLedger's state -- the work-in-progress block -- to accept or reject deposits
      * depending on whether the [[dappLedger]] Actor can successfully register the deposit,
      */
    private def registerUserDeposit(req: UserEvent.DepositEvent): IO[Unit] = {
        import req.*

        val rejectEvent = (e: L1LedgerM.Error | L2LedgerError) =>
            for {
                oldState <- unsafeGetProducing
                newState = oldState
                    .focus(_.userEventState.events)
                    .modify(_.appended((eventId, Invalid)))
                _ <- state.set(newState)
                _ = logger.debug(s"registerUserDeposit failure: $e")
            } yield ()

        for {
            blockStartTime <- unsafeGetProducing.map(_.startTime)
            _ <- this.runL1LedgerM(
              action = L1LedgerM.registerDeposit(req, blockStartTime),
              // Left == deposit rejected
              // FIXME: This should probably be returned as sum type in the Right
              onFailure = rejectEvent,
              onSuccess = (depositProduced: DepositUtxo, refundTx: RefundTx.PostDated) =>
                  for {
                      oldState <- unsafeGetProducing

                      // Create the L2 Ledger Action for deposit registration from
                      // - The user event
                      // - The current state
                      // - The parse result of the deposit tx
                      l2LedgerEvent = L2LedgerEvent.DepositEventRegistration(
                        eventId = eventId,
                        blockNumber = oldState.nextBlockNumber,
                        blockCreationStartTime = oldState.startTime,
                        depositUtxoId = depositProduced.utxoId,
                        depositFee = req.depositFee,
                        depositL2Value = req.l2Value,
                        l2Payload = req.l2Payload
                      )

                      _ <- this.runL2LedgerAction(
                        action = l2Ledger.L2LedgerAction.fromL2LedgerEvent(l2LedgerEvent),
                        onFailure = rejectEvent,
                        onSuccess = _ =>
                            for {
                                oldState <- unsafeGetProducing
                                newState = oldState
                                    .focus(_.userEventState.events)
                                    .modify(_.appended((eventId, Valid)))
                                    .focus(_.userEventState.postDatedRefundTxs)
                                    .modify(_.appended(refundTx))
                                _ <- state.set(newState)
                            } yield ()
                      )

                  } yield ()
            )
        } yield ()
    }

    /** Update the current block with the result of passing the tx to the virtual ledger, as well as
      * updating ledgerEventsRequired
      */
    private def applyL2UserEvent(
        userL2Event: UserEvent.L2Event
    ): IO[Unit] = {
        import userL2Event.*
        for {
            p <- unsafeGetProducing
            l2Event: L2LedgerEvent.L2Event = L2LedgerEvent.L2Event(
              eventId = userL2Event.eventId,
              blockNumber = p.nextBlockNumber,
              blockCreationStartTime = p.startTime,
              l2Payload = userL2Event.l2Payload
            )
            _ <- runL2LedgerAction(
              action = this.l2Ledger.L2LedgerAction.fromL2LedgerEvent(l2Event),
              // Invalid transaction continuation
              onFailure = _ =>
                  for {
                      p <- unsafeGetProducing
                      newState = p
                          .focus(_.userEventState.events)
                          .modify(_.appended((eventId, Invalid)))
                      _ <- state.set(newState)
                  } yield (),
              // Valid transaction continuation
              onSuccess = payoutObligations =>
                  for {
                      p <- unsafeGetProducing
                      newState = p
                          .focus(_.userEventState.events)
                          .modify(_.appended((eventId, Valid)))
                      _ <- state.set(newState)
                  } yield ()
            )
        } yield ()
    }

    /** Moves the state of the JointLedger from "Done" to "Producing", setting the time and
      * ledgerEventsRequired appropriately, while initializing all other fields.
      * @return
      */
    private def startBlock(args: StartBlock): IO[Unit] = {
        import args.*
        for {
            d <- unsafeGetDone
            _ <- state.set(
              Producing(
                previousBlock = d.producedBlock,
                competingFallbackValidityStart = d.lastFallbackValidityStart,
                startTime = blockCreationTime,
                userEventState = UserEventState(
                  events = List.empty,
                  postDatedRefundTxs = Vector.empty
                ),
                l1LedgerState = d.l1LedgerState,
                l2LedgerState = L2LedgerState.empty,
                evacuationMap = d.evacuationMap
              )
            )
        } yield ()
    }

    /** Complete a Minor or Major block If
      * @return
      */
    private def completeBlockRegular(args: CompleteBlockRegular): IO[Unit] = {
        import args.*

        /** @return
          *   Queue order:
          *   - eligible for absorption
          *   - ineligible for absorption - (immature + mature but non-existent)
          *   - rejected
          */

        def partitionDeposits(
            depositsMap: DepositsMap,
            blockStartTime: QuantizedInstant,
            settlementValidityEnd: QuantizedInstant
        ): IO[
          (
              DepositsMap,
              DepositsMap,
              DepositsMap
          )
        ] = for {
            _ <- IO.pure(())

            _ = logger.trace(
              s"partitionDeposits: deposits: ${depositsMap.treeMap.values.map(_.map(_._1))}, " +
                  s"blockStartTime=$blockStartTime, " +
                  s"settlementValidityEnd=$settlementValidityEnd"
            )

            ret = depositsMap.foldLeft(
              (
                DepositsMap.empty,
                DepositsMap.empty,
                DepositsMap.empty
              )
            ) { case (acc0, (_absorptionStartTime, depositQueue)) =>
                depositQueue.foldLeft(acc0) { case (acc, event @ (_eventId, deposit)) =>
                    val depositValidityEnd = deposit.submissionDeadline
                    val depositAbsorptionStart =
                        txTiming.depositAbsorptionStartTime(depositValidityEnd)
                    val depositAbsorptionEnd =
                        txTiming.depositAbsorptionEndTime(depositValidityEnd)

                    // Maturity. The deposit is mature if its absorption start time is no later
                    // than the block brief’s creation end time.
                    // TODO: use end time, not start time
                    val isMature = depositAbsorptionStart <= blockStartTime

                    // Non-expiry. The deposit is non-expired if its absorption end time is
                    // no earlier than the competing fallback’s validity start time.

                    // NB: Here, we can't use submission deadline, which is currently the same thing as deposit tx
                    // ttl to decide whether we should reject a deposit - even if the submission time is over,
                    // the state we observe may fall behind for any reason (network congestion, fork).
                    // So we can only use absorption end time to decide it's time to reject a deposit.
                    val isExpired = settlementValidityEnd > depositAbsorptionEnd

                    // TODO: The fast-consensus leader sees the deposit on the L1 blockchain when he ends his leadership term.
                    // If a follower is reproducing the block brief, replace this condition with:
                    // the follower sees the deposit on the L1 blockchain when he finishes verifying
                    // the block brief’s events.
                    val isExistent = pollResults.contains(deposit.toUtxo.input)

                    val isEligible = !isExpired && isMature && isExistent

                    // A deposit is ineligible and must be rejected if
                    // - the deposit is expired, or
                    // - if it’s mature and non-expired but doesn’t exist on L1.
                    val isRejected = isExpired || (isMature && !isExistent)

                    val isIneligible = !isExpired && (!isMature || !isExistent)

                    logger.trace(
                      s"deposit: ${deposit._1}, " +
                          s"depositValidityEnd=$depositValidityEnd, " +
                          s"depositAbsorptionStart=$depositAbsorptionStart, " +
                          s"depositAbsorptionEnd=$depositAbsorptionEnd, " +
                          s"isMature=$isMature, " +
                          s"isExpired=$isExpired, " +
                          s"isExistent=$isExistent, " +
                          s"isEligible=$isEligible, " +
                          s"isRejected=$isRejected, " +
                          s"isIneligible=$isIneligible"
                    )

                    if isEligible
                    then acc.focus(_._1).modify(_.appended(event, config.txTiming))
                    else if isIneligible
                    then acc.focus(_._2).modify(_.appended(event, config.txTiming))
                    else if isExpired
                    then acc.focus(_._3).modify(_.appended(event, config.txTiming))
                    else throw RuntimeException("Deposit decision function is not total")
                }
            }
        } yield (ret._1, ret._2, ret._3)

        /** KZG commitment + block brief (which is a bit strange)
          */
        def mkBlockBrief(
            absorbedDeposits: Queue[(LedgerEventId, DepositUtxo)],
            rejectedDeposits: Queue[(LedgerEventId, DepositUtxo)],
        ): IO[BlockBrief.Intermediate] = for {

            p <- unsafeGetProducing

            previousHeader = p.previousBlock.header
            blockWithdrawnUtxos = p.l2LedgerState.payouts
            blockStartTime = p.startTime
            competingFallbackValidityStart = p.competingFallbackValidityStart
            events = p.userEventState.events

            _ = logger.trace(
              s"mkBlockBrief: previousHeader=$previousHeader\n" +
                  s"mkBlockBrief: blockWithdrawnUtxos=$blockWithdrawnUtxos\n" +
                  s"mkBlockBrief: blockStartTime=$blockStartTime\n" +
                  s"mkBlockBrief: competingFallbackValidityStart=$competingFallbackValidityStart\n" +
                  s"mkBlockBrief: events=$events"
            )

            // Block header
            headerIntermediate: BlockHeader.Intermediate <-
                if absorbedDeposits.isEmpty && blockWithdrawnUtxos.isEmpty
                then
                    IO.pure(
                      previousHeader.nextHeaderIntermediate(
                        txTiming,
                        blockStartTime,
                        competingFallbackValidityStart,
                        // TODO: We want this to be done in a separate actor in the future
                        // this doesn't include genesis
                        applyDiffs(p.evacuationMap, p.l2LedgerState.diffs).kzgCommitment
                      )
                    )
                else {
                    val depositEventDecisions: L2LedgerEvent.DepositEventDecisions =
                        L2LedgerEvent.DepositEventDecisions(
                          p.nextBlockNumber,
                          // Why vector and not Queue?
                          Vector.from(absorbedDeposits.map(_._1)),
                          Vector.from(rejectedDeposits.map(_._1))
                        )
                    for {
                        newL2State <- runL2LedgerAction(
                          this.l2Ledger.L2LedgerAction.fromL2LedgerEvent(depositEventDecisions)
                        )
                        p <- unsafeGetProducing
                        newEvacuationMap = applyDiffs(p.evacuationMap, newL2State.diffs)
                        _ <- state.set(p.focus(_.evacuationMap).replace(newEvacuationMap))
                        // TODO: We want this to be done in a separate actor in the future
                        kzgCommitment = newEvacuationMap.kzgCommitment
                        headerIntermediate = previousHeader.nextHeaderMajor(
                          blockStartTime,
                          kzgCommitment
                        )
                    } yield headerIntermediate
                }

            // Block brief
            blockBrief: BlockBrief.Intermediate = headerIntermediate match {
                case header: BlockHeader.Minor =>
                    val blockBody = BlockBody.Minor(events, rejectedDeposits.map(_._1).toList)
                    BlockBrief.Minor(header, blockBody)
                case header: BlockHeader.Major =>
                    val blockBody = BlockBody.Major(
                      events,
                      absorbedDeposits.map(_._1).toList,
                      rejectedDeposits.map(_._1).toList
                    )
                    BlockBrief.Major(header, blockBody)
            }
        } yield blockBrief

        def mkBlockEffects(
            next: BlockBrief.Intermediate,
            absorbedDeposits: Queue[(LedgerEventId, DepositUtxo)],
            postDatedRefundTxs: List[RefundTx.PostDated]
        ): IO[Block.Unsigned.Next] = for {
            ret <- next match {
                case blockBrief @ BlockBrief.Minor(header, _) =>
                    val blockEffects = BlockEffects.Unsigned.Minor(
                      headerSerialized = header.onchainMsg,
                      postDatedRefundTxs = postDatedRefundTxs
                    )
                    IO.pure(Block.Unsigned.Minor(blockBrief, blockEffects))
                case blockBrief @ BlockBrief.Major(header, _) =>
                    for {
                        p <- unsafeGetProducing

                        payoutObligations = p.l2LedgerState.payouts

                        settlementTxSeq <- this.runL1LedgerM(
                          L1LedgerM.mkSettlementTxSeq(
                            nextKzg = header.kzgCommitment,
                            absorbedDeposits = absorbedDeposits,
                            payoutObligations = payoutObligations,
                            blockCreatedOn = header.startTime,
                            competingFallbackValidityStart =
                                txTiming.newFallbackStartTime(header.startTime)
                          ),
                          onSuccess = IO.pure
                        )

                        blockEffects = BlockEffects.Unsigned.Major(
                          settlementTx = settlementTxSeq.settlementTx,
                          fallbackTx = settlementTxSeq.fallbackTx,
                          rolloutTxs = settlementTxSeq.rolloutTxs,
                          postDatedRefundTxs = postDatedRefundTxs
                        )
                    } yield Block.Unsigned.Major(blockBrief, blockEffects)
            }
        } yield ret

        // ===================================
        // Finally, the body of completeBlockRegular
        // ===================================

        import config.txTiming

        for {
            producing <- unsafeGetProducing

            ret <- partitionDeposits(
              depositsMap = producing.l1LedgerState.deposits,
              blockStartTime = producing.startTime,
              settlementValidityEnd =
                  txTiming.newSettlementEndTime(producing.competingFallbackValidityStart)
            )

            (eligible, ineligible, rejected) = ret

            (absorbedDeposits, unabsorbedDeposits) = eligible.splitAt(maxDepositsAbsorbedPerBlock)

            _ = logger.trace(
              s"joint ledger: absorbed=$absorbedDeposits" + "\n" +
                  s"joint ledger: unabsorbed=$unabsorbedDeposits" + "\n" +
                  s"joint ledger: ineligible=$ineligible" + "\n" +
                  s"joint ledger: rejected=$rejected" + "\n"
            )

            blockBrief <- mkBlockBrief(
              absorbedDeposits = Queue.from(absorbedDeposits.flatValues),
              rejectedDeposits = Queue.from(rejected.flatValues)
            )

            block <- mkBlockEffects(
              blockBrief,
              Queue.from(absorbedDeposits.flatValues),
              producing.userEventState.postDatedRefundTxs.toList
            )

            _ <- checkReferenceBlock(referenceBlockBrief, block)

            // Block is done

            // This is also sort of "handling", but internal  to ledgers
            _ <- this.runL1LedgerM(
              L1LedgerM.handleBlockBrief(unabsorbedDeposits ++ ineligible),
              onSuccess = IO.pure
            )

            // Tell others about the block
            _ <- handleBlock(block, finalizationLocallyTriggered)
        } yield ()
    }

    // Block completion Signal is provided to the joint ledger when the block weaver says it's time.
    // If it's a final block, we don't pass poll results from the cardano liaison. Otherwise, we do.
    // We need to:
    //   - Compile the information from the transient fields into a block
    //   - put it into "previous block"
    //   - wipe the "transient fields"
    // If a "reference block" is passed, this means that the block we produce must be equal to the reference block.
    // If the produced block is NOT equal to a passed reference block, then:
    //   - Consensus is broken
    //   - Send a panic to the multisig regime manager in a suicide note
    def completeBlockFinal(args: CompleteBlockFinal): IO[Unit] = {
        import args.*
        import config.txTiming

        for {
            p <- unsafeGetProducing

            finalizationTxSeq <- this.runL1LedgerM(
              L1LedgerM.finalizeLedger(
                payoutObligationsRemaining = Vector.from(
                  p.evacuationMap.evacuationMap.map((_, o) => Payout.Obligation(o))
                ),
                blockCreatedOn = p.startTime,
                competingFallbackValidityStart = p.startTime
                    + txTiming.minSettlementDuration
                    + txTiming.inactivityMarginDuration
                    + txTiming.silenceDuration,
              ),
              onSuccess = IO.pure
            )

            block: Block.Unsigned.Final = {
                import p.userEventState.*
                val blockHeader = p.previousBlock.header.nextHeaderFinal(p.startTime)

                val blockBody = BlockBody.Final(
                  events = events,
                  // Final block should reject all the deposits known.
                  depositsRefunded = p.l1LedgerState.deposits.flatEvents.toList
                )

                val blockBrief = BlockBrief.Final(blockHeader, blockBody)

                val blockEffects = BlockEffects.Unsigned.Final(
                  finalizationTx = finalizationTxSeq.finalizationTx,
                  rolloutTxs = finalizationTxSeq.rolloutTxs
                )

                Block.Unsigned.Final(blockBrief, blockEffects)
            }

            _ <- checkReferenceBlock(referenceBlockBrief, block)
            _ <- handleBlock(block, false)

        } yield ()
    }

    /** When a block is finished, we handle it by:
      *   - sending the pure (with no effects) block to peer liaisons for circulation
      *   - sending the block brief to the peer liaisons
      *   - sending the block to the consensus actor
      *   - signing block's effects and producing our own set of acks
      *   - sending block's ack(s) to the consensus actor
      */
    private def handleBlock(
        block: Block.Unsigned.Next,
        localFinalization: Boolean
    ): IO[Unit] =
        for {
            conn <- getConnections
            acks = ownHeadWallet.mkAcks(block, localFinalization)
            _ <- (conn.peerLiaisons ! block.blockBriefNext).parallel
            _ <- conn.consensusActor ! block
            _ <- IO.traverse_(acks)(ack => conn.consensusActor ! ack)
        } yield ()

    private def checkReferenceBlock(
        expectedBlockBrief: Option[BlockBrief],
        actualBlock: Block
    ): IO[Unit] = for {
        p <- unsafeGetProducing
        fallbackValidityStart: QuantizedInstant =
            actualBlock match {
                case major: Block.Unsigned.Major =>
                    major.effects.fallbackTx.validityStart
                case _ => p.competingFallbackValidityStart
            }

        _ <- expectedBlockBrief match {
            case Some(refBlock) if refBlock == actualBlock =>
                state.set(
                  Done(
                    actualBlock.block,
                    fallbackValidityStart,
                    p.l1LedgerState,
                    p.evacuationMap
                  )
                )
            case Some(_) =>
                panic(
                  "Reference block didn't match actual block; consensus is broken."
                ) >> context.self.stop
            case None =>
                state.set(
                  Done(
                    actualBlock.block,
                    fallbackValidityStart,
                    p.l1LedgerState,
                    p.evacuationMap
                  )
                )
        }
    } yield ()

    // Sends a panic to the multisig regime manager, indicating that the node cannot proceed any more
    // TODO: Implement better, it should be typed and the multisig regime manager should be able to pattern match
    private def panic(msg: String): IO[Unit] = throw new RuntimeException(msg)
}

/** ==Hydrozoa's joint ledger on Cardano in the multisig regime==
  *
  * Hydrozoa's joint ledger connects its dapp ledger to its virtual ledger. It dispatches some state
  * transitions to them individually, but it also periodically reconciles state transitions across
  * them to keep them aligned.
  */
object JointLedger {
    type Handle = ActorRef[IO, Requests.Request]

    type Config = HeadConfig.Section & OwnHeadPeerPrivate.Section

    final case class Connections(
        consensusActor: ConsensusActor.Handle,
        peerLiaisons: List[PeerLiaison.Handle]
    )

    final case class CompleteBlockError() extends Throwable

    object Requests {
        type Request =
            // RegisterDeposit is exactly the DappLedger type, we're simply forwarding it through.
            // Does this mean we should wrap it?
            UserEvent | StartBlock | CompleteBlockRegular | CompleteBlockFinal | GetState.Sync

        case class StartBlock(
            blockNum: BlockNumber,
            blockCreationTime: QuantizedInstant
        )

        /** @param referenceBlockBrief
          *   provided by the BlockWeaver when it is in follower mode. When the joint ledger is
          *   finished reproducing the block, it compares against this reference block to determine
          *   whether the leader properly constructed the original block.
          * @param pollResults
          *   there are two reasons to have it here:
          *   - pollResults are absent upon weaver's start time. Passing it here may improve things.
          *   - pollResults are needed only when we are finishing a regular (non-final) block.
          * @param finalizationLocallyTriggered
          *   this flag indicates that head finalization request was received LOCALLY and the next
          *   block should be the final block which is indicated by setting the flag
          *   `finalizationRequested` in the block acknowledgement
          */
        case class CompleteBlockRegular(
            referenceBlockBrief: Option[BlockBrief.Intermediate],
            pollResults: Set[TransactionInput],
            finalizationLocallyTriggered: Boolean
        )

        case class CompleteBlockFinal(
            referenceBlockBrief: Option[BlockBrief.Final],
        )

        case object GetState extends SyncRequest[IO, GetState.type, State] {
            type Sync = SyncRequest.Envelope[IO, GetState.type, State]

            def ?: : this.Send = SyncRequest.send(_, this)
        }

    }

    sealed trait State {
        val l1LedgerState: L1LedgerM.State
        val evacuationMap: EvacuationMap
    }

    final case class Done(
        producedBlock: Block,
        // None for the first block
        lastFallbackValidityStart: QuantizedInstant,
        override val l1LedgerState: L1LedgerM.State,
        override val evacuationMap: EvacuationMap
    ) extends State

    final case class Producing(
        override val l1LedgerState: L1LedgerM.State,
        override val evacuationMap: EvacuationMap,
        l2LedgerState: L2LedgerState,
        previousBlock: Block,
        // None for the first block
        competingFallbackValidityStart: QuantizedInstant,
        startTime: QuantizedInstant,
        userEventState: UserEventState
    ) extends State {
        val nextBlockNumber: BlockNumber.BlockNumber = previousBlock.blockNum.increment
    }
}
