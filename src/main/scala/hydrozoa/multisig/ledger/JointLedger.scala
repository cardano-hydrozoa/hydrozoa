package hydrozoa.multisig.ledger

import cats.effect.{IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastOps
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.lib.actor.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.{ConsensusActor, PeerLiaison}
import hydrozoa.multisig.ledger.DappLedgerM.runDappLedgerM
import hydrozoa.multisig.ledger.JointLedger.*
import hydrozoa.multisig.ledger.JointLedger.Requests.*
import hydrozoa.multisig.ledger.VirtualLedgerM.runVirtualLedgerM
import hydrozoa.multisig.ledger.block.{Block, BlockBody, BlockBrief, BlockEffects, BlockHeader, BlockNumber}
import hydrozoa.multisig.ledger.dapp.txseq.{FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.event.LedgerEvent.*
import hydrozoa.multisig.ledger.event.LedgerEventId.ValidityFlag
import hydrozoa.multisig.ledger.event.LedgerEventId.ValidityFlag.{Invalid, Valid}
import hydrozoa.multisig.ledger.event.{LedgerEvent, LedgerEventId}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.tx.{GenesisObligation, L2Genesis}
import monocle.Focus.focus
import scala.collection.immutable.{Queue, TreeMap}
import scala.math.Ordered.orderingToOrdered
import scalus.cardano.ledger.{AssetName, TransactionHash, TransactionInput}
import scalus.uplc.builtin.{ByteString, platform}

// Fields of a work-in-progress block, with an additional field for dealing with withdrawn utxos
private case class TransientFields(
    events: List[(LedgerEventId, ValidityFlag)],
    blockWithdrawnUtxos: Vector[Payout.Obligation]
)

// NOTE: Joint ledger is created by the MultisigManager.
// NOTE: As of 2025-11-16, George says BlockWeaver should be the ONLY actor calling the joint ledger
final case class JointLedger(
    config: Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | JointLedger.Connections,
) extends Actor[IO, Requests.Request] {
    import config.*

    private val logger = Logging.logger("JointLedger")

    private val connections = Ref.unsafe[IO, Option[Connections]](None)

    val state: Ref[IO, JointLedger.State] =
        Ref.unsafe[IO, JointLedger.State](
          Done(
            producedBlock = config.initialBlock,
            lastFallbackValidityStart = config.initialFallbackTx.validityStart,
            dappLedgerState =
                DappLedgerM.State(config.initializationTx.treasuryProduced, Queue.empty),
            virtualLedgerState = VirtualLedgerM.State.apply(config.initialEvacuationMap)
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
        case e: LedgerEvent          => registerLedgerEvent(e)
        case s: StartBlock           => startBlock(s)
        case c: CompleteBlockRegular => completeBlockRegular(c)
        case f: CompleteBlockFinal   => completeBlockFinal(f)
        case req: SyncRequest.Any =>
            req.request match {
                case r: GetState.type => r.handleSync(req, _ => state.get)
            }
    }

    private def registerLedgerEvent(e: LedgerEvent): IO[Unit] = {
        e match {
            case req: DepositEvent => registerDeposit(req)
            case tx: L2TxEvent     => applyInternalTxL2(tx)
        }
    }

    /** Update the JointLedger's state -- the work-in-progress block -- to accept or reject deposits
      * depending on whether the [[dappLedger]] Actor can successfully register the deposit,
      */
    private def registerDeposit(req: DepositEvent): IO[Unit] = {
        import req.*
        for {
            blockStartTime <- unsafeGetProducing.map(_.startTime)
            _ <- this.runDappLedgerM(
              action = DappLedgerM.registerDeposit(req, blockStartTime),
              // Left == deposit rejected
              // FIXME: This should probably be returned as sum type in the Right
              onFailure = e =>
                  for {
                      oldState <- unsafeGetProducing
                      newState = oldState
                          .focus(_.nextBlockData.events)
                          .modify(_.appended((eventId, Invalid)))
                      _ <- state.set(newState)
                      _ = logger.debug(s"registerDeposit failure: $e")
                  } yield (),
              onSuccess = _s =>
                  for {
                      oldState <- unsafeGetProducing
                      newState = oldState
                          .focus(_.nextBlockData.events)
                          .modify(_.appended((eventId, Valid)))
                      _ <- state.set(newState)
                  } yield ()
            )
        } yield ()
    }

    /** Update the current block with the result of passing the tx to the virtual ledger, as well as
      * updating ledgerEventsRequired
      */
    private def applyInternalTxL2(
        txEvent: L2TxEvent
    ): IO[Unit] = {
        import txEvent.*

        for {
            p <- unsafeGetProducing
            _ <- this.runVirtualLedgerM(
              action = VirtualLedgerM.applyInternalTx(tx, p.startTime),
              // Invalid transaction continuation
              onFailure = _ =>
                  for {
                      p <- unsafeGetProducing
                      newState = p
                          .focus(_.nextBlockData.events)
                          .modify(_.appended((eventId, Invalid)))
                      _ <- state.set(newState)
                  } yield (),
              // Valid transaction continuation
              onSuccess = payoutObligations =>
                  for {
                      p <- unsafeGetProducing
                      newState = p
                          .focus(_.nextBlockData.events)
                          .modify(_.appended((eventId, Valid)))
                          .focus(_.nextBlockData.blockWithdrawnUtxos)
                          .modify(v => v ++ payoutObligations)
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
                TransientFields(
                  events = List.empty,
                  blockWithdrawnUtxos = Vector.empty
                ),
                dappLedgerState = d.dappLedgerState,
                virtualLedgerState = d.virtualLedgerState
              )
            )
        } yield ()
    }

    /** Complete a Minor or Major block If
      * @return
      */
    private def completeBlockRegular(args: CompleteBlockRegular): IO[Unit] = {
        import args.*

        /** TODO: partitioning probably isn't the fastest way, because it will inspect each element
          * of the queue. But I don't recall if we assume the queue is sorted according to maturity
          * time, so I'll go with this for now. If it is sorted, there's almost certainly a more
          * efficient function. TODO: on the other hand: we cannot avoid handling them element-wise
          * since we also need to check whether a deposit utxo is present
          *
          * @return
          *   Queue order:
          *   - eligible for absorption
          *   - ineligible for absorption - (inmature + mature but non-existent)
          *   - rejected
          */

        def partitionDeposits(
            depositStream: Queue[(LedgerEventId, DepositUtxo)],
            blockStartTime: QuantizedInstant,
            settlementValidityEnd: QuantizedInstant
        ): IO[
          (
              Queue[(LedgerEventId, DepositUtxo)],
              Queue[(LedgerEventId, DepositUtxo)],
              Queue[(LedgerEventId, DepositUtxo)]
          )
        ] = for {
            _ <- IO.pure(())

            deposits = sortDepositStream(depositStream, config.txTiming)

            _ = logger.trace(
              s"partitionDeposits: deposits: ${deposits.map(_._1)}, " +
                  s"blockStartTime=$blockStartTime, " +
                  s"settlementValidityEnd=$settlementValidityEnd"
            )

            ret = deposits.foldLeft(
              (
                Queue.empty[(LedgerEventId, DepositUtxo)],
                Queue.empty[(LedgerEventId, DepositUtxo)],
                Queue.empty[(LedgerEventId, DepositUtxo)]
              )
            )((acc, deposit) =>

                val depositValidityEnd = deposit._2.submissionDeadline
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
                val isExistent = pollResults.contains(deposit._2.toUtxo.input)

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
                then acc.focus(_._1).modify(_.appended(deposit))
                else if isIneligible
                then acc.focus(_._2).modify(_.appended(deposit))
                else if isExpired
                then acc.focus(_._3).modify(_.appended(deposit))
                else throw RuntimeException("Deposit decision function is not total")
            )
        } yield (ret._1, ret._2, ret._3)

        /** KZG commitment + block brief (which is a bit strange)
          */
        def mkBlockBrief(
            absorbedDeposits: Queue[(LedgerEventId, DepositUtxo)],
            rejectedDeposits: Queue[(LedgerEventId, DepositUtxo)],
        ): IO[(BlockBrief.Intermediate, Option[L2Genesis])] = for {

            p <- unsafeGetProducing

            previousHeader = p.previousBlock.header
            blockWithdrawnUtxos = p.nextBlockData.blockWithdrawnUtxos
            blockStartTime = p.startTime
            competingFallbackValidityStart = p.competingFallbackValidityStart
            events = p.nextBlockData.events

            _ = logger.trace(
              s"mkBlockBrief: previousHeader=${previousHeader}\n" +
                  s"mkBlockBrief: blockWithdrawnUtxos=${blockWithdrawnUtxos}\n" +
                  s"mkBlockBrief: blockStartTime=${blockStartTime}\n" +
                  s"mkBlockBrief: competingFallbackValidityStart=${competingFallbackValidityStart}\n" +
                  s"mkBlockBrief: events=${events}"
            )

            // Block header
            ret <-

                if absorbedDeposits.isEmpty && blockWithdrawnUtxos.isEmpty
                then
                    IO.pure(
                      (
                        previousHeader.nextHeaderIntermediate(
                          txTiming,
                          blockStartTime,
                          competingFallbackValidityStart,
                          // this doesn't include genesis
                          p.virtualLedgerState.kzgCommitment
                        ),
                        None
                      )
                    )
                else
                    for {
                        ret <-
                            if absorbedDeposits.isEmpty
                            then
                                IO.pure(
                                  (
                                    p.virtualLedgerState.kzgCommitment,
                                    None
                                  )
                                )
                            else {
                                val treasuryToSpend = p.dappLedgerState.treasury
                                val genesisObligations: Queue[GenesisObligation] =
                                    absorbedDeposits
                                        .map(_._2.virtualOutputs)
                                        .foldLeft(Queue.empty)((acc, ob) =>
                                            acc.appendedAll(ob.toList)
                                        )
                                val genesisEvent = L2Genesis(
                                  genesisObligations,
                                  mkGenesisId(
                                    headTokenNames.treasuryTokenName,
                                    treasuryToSpend.datum.versionMajor.toInt + 1
                                  )
                                )

                                for {
                                    kzgCommitment <- this.runVirtualLedgerM(
                                      VirtualLedgerM.mockApplyGenesis(genesisEvent)
                                    )
                                } yield (kzgCommitment, Some(genesisEvent))
                            }
                        (kzgCommitment, mbGenesisEvent) = ret
                        headerIntermediate = previousHeader.nextHeaderMajor(
                          blockStartTime,
                          kzgCommitment
                        )
                    } yield (headerIntermediate, mbGenesisEvent)

            (headerIntermediate, mbGenesisEvent) = ret

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
        } yield (blockBrief, mbGenesisEvent)

        def mkBlockEffects(
            next: BlockBrief.Intermediate,
            absorbedDeposits: Queue[(LedgerEventId, DepositUtxo)]
        ): IO[Block.Unsigned.Next] = for {
            ret <- next match {
                case blockBrief @ BlockBrief.Minor(header, _) =>
                    val blockEffects = BlockEffects.Unsigned.Minor(
                      headerSerialized = header.onchainMsg,
                      postDatedRefundTxs = List() // FIXME: Where are they?
                    )
                    IO.pure(Block.Unsigned.Minor(blockBrief, blockEffects))
                case blockBrief @ BlockBrief.Major(header, _) =>
                    for {
                        p <- unsafeGetProducing

                        payoutObligations = p.nextBlockData.blockWithdrawnUtxos

                        settlementTxSeq <- this.runDappLedgerM(
                          DappLedgerM.mkSettlementTxSeq(
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
                          postDatedRefundTxs = List() // FIXME: Where are they?
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
              depositStream = producing.dappLedgerState.deposits,
              blockStartTime = producing.startTime,
              settlementValidityEnd =
                  txTiming.newSettlementEndTime(producing.competingFallbackValidityStart)
            )

            (eligible, ineligible, rejected) = ret

            // TODO: wiring point for Peter
            (absorbedDeposits, unabsorbedDeposits) = eligible.splitAt(maxDepositsAbsorbedPerBlock)

            _ = logger.trace(
              s"joint ledger: absorbed=${absorbedDeposits}" + "\n" +
                  s"joint ledger: unabsorbed=${unabsorbedDeposits}" + "\n" +
                  s"joint ledger: ineligible=${ineligible}" + "\n" +
                  s"joint ledger: rejected=${rejected}" + "\n"
            )

            ret <- mkBlockBrief(
              absorbedDeposits = absorbedDeposits,
              rejectedDeposits = rejected
            )
            (blockBrief, mbGenesisEvent) = ret

            block <- mkBlockEffects(blockBrief, absorbedDeposits)

            _ <- checkReferenceBlock(referenceBlockBrief, block)

            // Block is done

            // This is also sort of "handling", but internal  to ledgers
            _ <- this.runDappLedgerM(
              DappLedgerM.handleBlockBrief(unabsorbedDeposits ++ ineligible),
              onSuccess = IO.pure
            )

            _ <- mbGenesisEvent.fold(IO.pure(())) { l2Genesis =>
                this.runVirtualLedgerM(VirtualLedgerM.applyGenesisEvent(l2Genesis))
            }

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

            finalizationTxSeq <- this.runDappLedgerM(
              DappLedgerM.finalizeLedger(
                payoutObligationsRemaining = Vector.from(
                  p.virtualLedgerState.evacuationMap.evacMap.map((i, o) => Payout.Obligation(o))
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
                import p.nextBlockData.*
                val blockHeader = p.previousBlock.header.nextHeaderFinal(p.startTime)

                val blockBody = BlockBody.Final(
                  events = events,
                  // Final block should reject all the deposits known.
                  depositsRefunded = p.dappLedgerState.deposits.map(_._1).toList
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
                    p.dappLedgerState,
                    p.virtualLedgerState
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
                    p.dappLedgerState,
                    p.virtualLedgerState
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

    /** @param depositStream
      *   The stream of deposits sorted according to the total ordering.
      * @param txTiming
      * @return
      *   The same stream, re-ordered according to absorption start time, with ties broken according
      *   to the ordering in which they appear in depositStream. The result is that if deposits are
      *   grouped into sequences [s1, s2, ...], according to identical absorption start time, each
      *   sequence s_i will be a (possibly non-contiguous) subsequence of depositStream.
      */
    def sortDepositStream(
        depositStream: Queue[(LedgerEventId, DepositUtxo)],
        txTiming: TxTiming
    ): Queue[(LedgerEventId, DepositUtxo)] = {
        val intermediateMap: TreeMap[QuantizedInstant, Queue[(LedgerEventId, DepositUtxo)]] =
            depositStream
                .foldLeft(TreeMap.empty[QuantizedInstant, Queue[(LedgerEventId, DepositUtxo)]])(
                  (acc, deposit) =>
                      acc.updatedWith(
                        txTiming.depositAbsorptionStartTime(deposit._2.submissionDeadline)
                      ) {
                          case None        => Some(Queue(deposit))
                          case Some(queue) => Some(queue.appended(deposit))
                      }
                )
        intermediateMap.foldLeft(Queue.empty) { case (q, (_, deposits)) => q ++ deposits }
    }

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
            LedgerEvent | StartBlock | CompleteBlockRegular | CompleteBlockFinal | GetState.Sync

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
        val dappLedgerState: DappLedgerM.State
        val virtualLedgerState: VirtualLedgerM.State
    }

    final case class Done(
        producedBlock: Block,
        // None for the first block
        lastFallbackValidityStart: QuantizedInstant,
        override val dappLedgerState: DappLedgerM.State,
        override val virtualLedgerState: VirtualLedgerM.State
    ) extends State

    final case class Producing(
        override val dappLedgerState: DappLedgerM.State,
        override val virtualLedgerState: VirtualLedgerM.State,
        previousBlock: Block,
        // None for the first block
        competingFallbackValidityStart: QuantizedInstant,
        startTime: QuantizedInstant,
        nextBlockData: TransientFields
    ) extends State

    def mkGenesisId(treasuryTokenName: AssetName, majorVersion: Int) =
        TransactionHash.fromByteString(
          platform.blake2b_256(
            treasuryTokenName.bytes ++
                ByteString.fromBigIntBigEndian(
                  BigInt(majorVersion)
                )
          )
        )

}
