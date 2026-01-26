package hydrozoa.multisig.ledger

import cats.effect.{IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastOps
import hydrozoa.config.EquityShares
import hydrozoa.lib.actor.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant, toEpochQuantizedInstant}
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.{ConsensusActor, PeerLiaison}
import hydrozoa.multisig.ledger.DappLedgerM.runDappLedgerM
import hydrozoa.multisig.ledger.JointLedger.*
import hydrozoa.multisig.ledger.JointLedger.Requests.*
import hydrozoa.multisig.ledger.VirtualLedgerM.runVirtualLedgerM
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.dapp.tx.TxTiming
import hydrozoa.multisig.ledger.dapp.txseq.{FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.{GenesisObligation, L2EventGenesis}
import hydrozoa.multisig.protocol.types
import hydrozoa.multisig.protocol.types.*
import hydrozoa.multisig.protocol.types.AckBlock.Number.neededToConfirm
import hydrozoa.multisig.protocol.types.AckBlock.{BlockAckSet, TxSignature}
import hydrozoa.multisig.protocol.types.Block.*
import hydrozoa.multisig.protocol.types.LedgerEvent.*
import hydrozoa.multisig.protocol.types.LedgerEventId.ValidityFlag
import hydrozoa.multisig.protocol.types.LedgerEventId.ValidityFlag.{Invalid, Valid}
import hydrozoa.{UtxoIdL1, Wallet}
import monocle.Focus.focus
import scala.collection.immutable.Queue
import scala.math.Ordered.orderingToOrdered
import scalus.builtin.{ByteString, platform}
import scalus.cardano.ledger.{CardanoInfo, Coin, TransactionHash}

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

    private val connections = Ref.unsafe[IO, Option[Connections]](None)

    val state: Ref[IO, JointLedger.State] =
        Ref.unsafe[IO, JointLedger.State](
          Done(
            producedBlock = Block.Initial(
              Block.Header.Initial(
                timeCreation = config.initialBlockTime,
                commitment = config.initialBlockKzg
              )
            ),
            lastFallbackValidityStart = config.initialFallbackValidityStart,
            dappLedgerState = DappLedgerM.State(config.initialTreasury, Queue.empty),
            virtualLedgerState = VirtualLedgerM.State.empty
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
            case req: RegisterDeposit => registerDeposit(req)
            case tx: TxL2Event        => applyInternalTxL2(tx)
        }
    }

    /** Update the JointLedger's state -- the work-in-progress block -- to accept or reject deposits
      * depending on whether the [[dappLedger]] Actor can successfully register the deposit,
      */
    private def registerDeposit(req: RegisterDeposit): IO[Unit] = {
        import req.*
        for {

            _ <- this.runDappLedgerM(
              action = DappLedgerM.registerDeposit(req),
              // Left == deposit rejected
              // FIXME: This should probably be returned as sum type in the Right
              onFailure = _ =>
                  for {
                      oldState <- unsafeGetProducing
                      newState = oldState
                          .focus(_.nextBlockData.events)
                          .modify(_.appended((eventId, Invalid)))
                      _ <- state.set(newState)
                  } yield (),
              onSuccess = _ =>
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
        txEvent: TxL2Event
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

        def augmentBlockMinor(
            depositsRefunded: List[LedgerEventId]
        ): IO[AugmentedBlock.Minor] = for {
            p <- unsafeGetProducing
            nextBlockBody: Block.Body.Minor = {
                import p.nextBlockData.*
                Block.Body.Minor(
                  events = events,
                  depositsRefunded = depositsRefunded
                )
            }

            kzgCommit = p.virtualLedgerState.kzgCommitment
            nextBlock: Block.Minor = p.previousBlock
                .nextBlock(
                  newBody = nextBlockBody,
                  newTime = p.startTime,
                  newCommitment = kzgCommit
                )
                .asInstanceOf[Block.Minor]

        } yield AugmentedBlock.Minor(
          nextBlock,
          BlockEffects.Minor(nextBlock.id, nextBlock.header, List.empty)
        )

        def augmentedBlockMajor(
            settleLedgerRes: DappLedgerM.SettleLedger.Result,
            refundedDeposits: List[LedgerEventId],
            absorbedDeposits: List[LedgerEventId]
        ): IO[AugmentedBlock.Major] =
            for {
                p <- unsafeGetProducing
                nextBlockBody: Block.Body.Major = {
                    import p.nextBlockData.*

                    Block.Body.Major(
                      events = events,
                      depositsAbsorbed = absorbedDeposits,
                      depositsRefunded = refundedDeposits
                    )
                }

                // FIXME: unsafe cast
                nextBlock: Block.Major = p.previousBlock
                    .nextBlock(
                      newBody = nextBlockBody,
                      newTime = p.startTime,
                      newCommitment = IArray.unsafeFromArray(
                        settleLedgerRes.settlementTxSeq.settlementTx.treasuryProduced.datum.commit.bytes
                      )
                    )
                    .asInstanceOf[Block.Major]

            } yield AugmentedBlock.Major(
              nextBlock,
              BlockEffects.Major(
                nextBlock.id,
                settlement = settleLedgerRes.settlementTxSeq.settlementTx,
                rollouts = settleLedgerRes.settlementTxSeq.mbRollouts,
                fallback = settleLedgerRes.fallBack,
                postDatedRefunds = List.empty
              )
            )

        def doSettlement(
            validDeposits: Queue[(LedgerEventId, DepositUtxo)],
            immatureDeposits: Queue[(LedgerEventId, DepositUtxo)],
        ): IO[DappLedgerM.SettleLedger.Result] =
            for {
                p <- unsafeGetProducing
                treasuryToSpend = p.dappLedgerState.treasury
                payoutObligations = p.nextBlockData.blockWithdrawnUtxos
                blockCreatedOn = p.startTime

                genesisObligations: Queue[GenesisObligation] =
                    validDeposits
                        .map(_._2.virtualOutputs)
                        .foldLeft(Queue.empty)((acc, ob) => acc.appendedAll(ob.toList))
                genesisEvent = L2EventGenesis(
                  genesisObligations,
                  TransactionHash.fromByteString(
                    platform.blake2b_256(
                      tokenNames.headTokenName.bytes ++
                          ByteString.fromBigIntBigEndian(
                            BigInt(treasuryToSpend.datum.versionMajor.toInt + 1)
                          )
                    )
                  )
                )

                nextKzg: KzgCommitment <- this.runVirtualLedgerM(
                  VirtualLedgerM.mockApplyGenesis(genesisEvent)
                )

                settleLedgerRes <- this.runDappLedgerM(
                  DappLedgerM.settleLedger(
                    nextKzg = nextKzg,
                    validDeposits = validDeposits,
                    payoutObligations = payoutObligations,
                    immatureDeposits = immatureDeposits,
                    blockCreatedOn = blockCreatedOn,
                    competingFallbackValidityStart = blockCreatedOn
                        + config.txTiming.minSettlementDuration
                        + config.txTiming.inactivityMarginDuration
                        + config.txTiming.silenceDuration,
                  ),
                  onSuccess = IO.pure
                )

                // Is it safe to apply this now?
                _ <- this.runVirtualLedgerM(VirtualLedgerM.applyGenesisEvent(genesisEvent))
            } yield settleLedgerRes

        import config.txTiming
        for {
            producing <- unsafeGetProducing

            // ===================================
            // Step 1: Figure out which deposits are valid and turn them into genesis obligations
            // ===================================

            // TODO: partitioning probably isn't the fastest way, because it will inspect each
            // element of the queue. But I don't recall if we assume the queue is sorted according to
            // maturity time, so I'll go with this for now. If it is sorted, there's almost certainly
            // a more efficient function.
            // TODO: Factor out
            depositsPartition = producing.dappLedgerState.deposits
                // Queue order: not yet mature, eligible for absorption, not eligible for absorption
                .foldLeft(
                  (
                    Queue.empty[(LedgerEventId, DepositUtxo)],
                    Queue.empty[(LedgerEventId, DepositUtxo)],
                    Queue.empty[(LedgerEventId, DepositUtxo)]
                  )
                )((acc, deposit) =>
                    val depositValidityEnd =
                        deposit._2.datum.refundInstructions.startTime
                            .toEpochQuantizedInstant(cardanoInfo.slotConfig)
                            - txTiming.depositMaturityDuration
                            - txTiming.depositAbsorptionDuration
                            - txTiming.silenceDuration

                    val depositAbsorptionStart: QuantizedInstant =
                        depositValidityEnd + txTiming.depositMaturityDuration

                    val depositAbsorptionEnd: QuantizedInstant =
                        depositAbsorptionStart + txTiming.depositAbsorptionDuration

                    val settlementValidityEnd: QuantizedInstant =
                        producing.competingFallbackValidityStart - txTiming.silenceDuration
                    {
                        if depositAbsorptionStart > producing.startTime
                        // Not yet mature
                        then acc.focus(_._1).modify(_.appended(deposit))
                        else if pollResults.contains(UtxoIdL1(deposit._2.toUtxo.input)) &&
                        (depositAbsorptionStart <= producing.startTime) &&
                        (settlementValidityEnd <= depositAbsorptionEnd)
                        // Eligible for absorption
                        then acc.focus(_._2).modify(_.appended(deposit))
                        else if ((depositAbsorptionStart <= producing.startTime)
                            && !pollResults.contains(UtxoIdL1(deposit._2.toUtxo.input))) ||
                        (settlementValidityEnd > depositAbsorptionEnd)
                        // Never eligible for absorption
                        then acc.focus(_._3).modify(_.appended(deposit))
                        // TODO: Is this total?
                        else throw RuntimeException("Don't know what to do with this deposit")
                    }
                )
            notYetMature = depositsPartition._1
            eligibleForAbsorption = depositsPartition._2
            neverEligibleForAbsorption = depositsPartition._3

            // For a description of timing, see GitHub issue #296
            forceUpgradeToMajor: Boolean =
                // First block is always major
                config.initialFallbackValidityStart == producing.competingFallbackValidityStart ||
                    // Upgrade if we are too close to fallback
                    (txTiming.minSettlementDuration >=
                        // FIXME: This time handling is a bit wonky because of the java <> scala mix
                        producing.competingFallbackValidityStart - txTiming.silenceDuration
                        - producing.startTime)

            isMinorBlock: Boolean =
                (eligibleForAbsorption.isEmpty && producing.nextBlockData.blockWithdrawnUtxos.isEmpty)
                    && (!forceUpgradeToMajor)

            augmentedBlock <-
                if isMinorBlock
                then
                    augmentBlockMinor(
                      depositsRefunded = neverEligibleForAbsorption.toList.map(_._1)
                    )
                else {
                    for {
                        settlementRes <- doSettlement(
                          validDeposits = eligibleForAbsorption,
                          immatureDeposits = notYetMature,
                        )
                        absorbedDeposits = eligibleForAbsorption.filter(deposit =>
                            settlementRes.settlementTxSeq.settlementTx.depositsSpent
                                .contains(deposit._2)
                        )
                        augBlockMajor <- augmentedBlockMajor(
                          settleLedgerRes = settlementRes,
                          refundedDeposits = neverEligibleForAbsorption.toList.map(_._1),
                          absorbedDeposits = absorbedDeposits.toList.map(_._1)
                        )
                    } yield augBlockMajor
                }

            _ <- checkReferenceBlock(referenceBlock, augmentedBlock)
            _ <- handleAugmentedBlock(augmentedBlock, finalizationLocallyTriggered)
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
                payoutObligationsRemaining = p.nextBlockData.blockWithdrawnUtxos,
                blockCreatedOn = p.startTime,
                competingFallbackValidityStart = p.startTime
                    + txTiming.minSettlementDuration
                    + txTiming.inactivityMarginDuration
                    + txTiming.silenceDuration,
              ),
              onSuccess = IO.pure
            )

            augmentedBlock: AugmentedBlock.Final = {
                import p.nextBlockData.*
                val nextBlockBody: Block.Body.Final = Block.Body.Final(
                  events = events,
                  // TODO: see comment in registerDeposit
                  // depositsRejected = depositsRejected ++ depositsRegistered,
                  depositsRefunded = List.empty // FIXME: currently not handling refunds
                )

                // FIXME: unsafe cast
                val nextBlock: Block.Final = p.previousBlock
                    .nextBlock(
                      nextBlockBody,
                      p.startTime,
                      IArray.empty[Byte]
                    )
                    .asInstanceOf[Block.Final]

                val blockEffects: BlockEffects.Final = {
                    import FinalizationTxSeq.*

                    BlockEffects.Final(
                      nextBlock.id,
                      finalizationTxSeq.finalizationTx,
                      rollouts = finalizationTxSeq.mbRollouts,
                      deinit = finalizationTxSeq.mbDeinit
                    )
                }

                AugmentedBlock.Final(nextBlock, blockEffects)
            }

            _ <- checkReferenceBlock(referenceBlock, augmentedBlock)
            _ <- handleAugmentedBlock(augmentedBlock, false)

        } yield ()
    }

    /** When a block is finished, we handle it by:
      *   - sending the pure (with no effects) block to peer liaisons for circulation
      *   - sending the augmented block with effects to the consensus actor
      *   - signing block's effects and producing our own set of acks
      *   - sending block's ack(s) to the consensus actor
      */
    private def handleAugmentedBlock(
        augmentedBlock: AugmentedBlock.Next,
        localFinalization: Boolean
    ): IO[Unit] =
        for {
            // _ <- blockSigner ! augmentedBlock
            conn <- getConnections
            _ <- (conn.peerLiaisons ! augmentedBlock.blockAsNext).parallel
            _ <- conn.consensusActor ! augmentedBlock
            ackSet <- signBlockEffects(
              augmentedBlock.blockAsNext.header,
              augmentedBlock.effects,
              localFinalization
            )
            _ <- IO.traverse_(ackSet.asList)(ack => conn.consensusActor ! ack)
        } yield ()

    private def signBlockEffects(
        header: Block.Header,
        effects: BlockEffects,
        localFinalization: Boolean
    ): IO[BlockAckSet] = effects match {
        case minor: BlockEffects.Minor =>
            val headerSignature = wallet.signMsg(minor.header.mkMessage)
            val refundSignatures =
                minor.postDatedRefunds
                    .map(r => wallet.signTx(r.tx))
                    .map(TxSignature.apply)

            IO.pure(
              AckBlock.Minor(
                id = AckBlock.Id.apply(peerId.peerNum, neededToConfirm(header)),
                blockNum = header.blockNum,
                headerSignature = headerSignature,
                postDatedRefunds = refundSignatures,
                finalizationRequested = localFinalization
              )
            )
        case major: BlockEffects.Major =>
            val fallbackSignature = TxSignature.apply(wallet.signTx(major.fallback.tx))
            val rolloutSignatures =
                major.rollouts
                    .map(r => wallet.signTx(r.tx))
                    .map(TxSignature.apply)
            val refundSignatures =
                major.postDatedRefunds
                    .map(r => wallet.signTx(r.tx))
                    .map(TxSignature.apply)
            val settlementSignature =
                TxSignature.apply(wallet.signTx(major.settlement.tx))
            val secondAckNumber = neededToConfirm(header)

            IO.pure(
              BlockAckSet.Major(
                AckBlock.Major1(
                  id = AckBlock.Id.apply(peerId.peerNum, secondAckNumber.decrement),
                  blockNum = header.blockNum,
                  fallback = fallbackSignature,
                  rollouts = rolloutSignatures,
                  postDatedRefunds = refundSignatures,
                  finalizationRequested = localFinalization
                ),
                AckBlock.Major2(
                  id = AckBlock.Id.apply(peerId.peerNum, secondAckNumber),
                  blockNum = header.blockNum,
                  settlement = settlementSignature
                )
              )
            )
        case f: BlockEffects.Final =>
            val rolloutSignatures =
                f.rollouts
                    .map(r => wallet.signTx(r.tx))
                    .map(TxSignature.apply)
            val deinitSignature =
                f.deinit.map(deinit => TxSignature.apply(wallet.signTx(deinit.tx)))

            val finalizationSignature =
                TxSignature.apply(wallet.signTx(f.finalization.tx))
            val secondAckNumber = neededToConfirm(header)

            IO.pure(
              BlockAckSet.Final(
                AckBlock.Final1(
                  id = AckBlock.Id.apply(peerId.peerNum, secondAckNumber.decrement),
                  blockNum = header.blockNum,
                  rollouts = rolloutSignatures,
                  deinit = deinitSignature
                ),
                AckBlock.Final2(
                  id = AckBlock.Id.apply(peerId.peerNum, secondAckNumber),
                  blockNum = header.blockNum,
                  finalization = finalizationSignature
                )
              )
            )
    }

    private def checkReferenceBlock(
        expectedBlock: Option[Block],
        actualBlock: AugmentedBlock
    ): IO[Unit] = for {
        p <- unsafeGetProducing
        fallbackValidityStart: QuantizedInstant =
            actualBlock match {
                case major: types.AugmentedBlock.Major =>
                    major.effects.fallback.validityStart
                case _ => p.competingFallbackValidityStart
            }

        _ <- expectedBlock match {
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

    type Handle = ActorRef[IO, Requests.Request]

    case class Config(
        peerId: Peer.Id,
        wallet: Wallet,
        initialBlockTime: QuantizedInstant,
        cardanoInfo: CardanoInfo,
        initialBlockKzg: KzgCommitment,
        txTiming: TxTiming,
        headMultisigScript: HeadMultisigScript,
        tallyFeeAllowance: Coin,
        equityShares: EquityShares,
        multisigRegimeUtxo: MultisigRegimeUtxo,
        votingDuration: QuantizedFiniteDuration,
        initialTreasury: MultisigTreasuryUtxo,
        tokenNames: TokenNames,
        initialFallbackValidityStart: QuantizedInstant
    )

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
            blockNum: Block.Number,
            blockCreationTime: QuantizedInstant
        )

        /** @param referenceBlock
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
            referenceBlock: Option[Block],
            pollResults: Set[UtxoIdL1],
            finalizationLocallyTriggered: Boolean
        )

        case class CompleteBlockFinal(
            referenceBlock: Option[Block],
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
}
