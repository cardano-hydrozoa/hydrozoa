package hydrozoa.multisig.ledger

import cats.effect.{IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.UtxoIdL1
import hydrozoa.config.EquityShares
import hydrozoa.lib.actor.*
import hydrozoa.multisig.ledger.DappLedgerM.runDappLedgerM
import hydrozoa.multisig.ledger.JointLedger.*
import hydrozoa.multisig.ledger.JointLedger.Requests.*
import hydrozoa.multisig.ledger.VirtualLedgerM.runVirtualLedgerM
import hydrozoa.multisig.ledger.dapp.tx.TxTiming.*
import hydrozoa.multisig.ledger.dapp.tx.{Tx, TxTiming}
import hydrozoa.multisig.ledger.dapp.txseq.{FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.{GenesisObligation, L2EventGenesis}
import hydrozoa.multisig.protocol.types.*
import hydrozoa.multisig.protocol.types.Block.*
import hydrozoa.multisig.protocol.types.LedgerEvent.*
import hydrozoa.multisig.protocol.{ConsensusProtocol, types}
import java.time.Instant
import monocle.Focus.focus
import scala.collection.immutable.Queue
import scala.concurrent.duration.FiniteDuration
import scalus.builtin.{ByteString, platform}
import scalus.cardano.ledger.{AssetName, Coin, TransactionHash}

// Fields of a work-in-progress block, with an additional field for dealing with withdrawn utxos
private case class TransientFields(
    events: List[(LedgerEventId, Boolean)],
    blockWithdrawnUtxos: Vector[Payout.Obligation]
)

// NOTE: Joint ledger is created by the MultisigManager.
// NOTE: As of 2025-11-16, George says BlockWeaver should be the ONLY actor calling the joint ledger
final case class JointLedger(
    peerLiaisons: Seq[ActorRef[IO, ConsensusProtocol.PeerLiaison.Request]],
    // private val cardanoLiaison
    // private val blockSigner
    //// Static config fields
    // in head config
    initialBlockTime: java.time.Instant,
    // derived
    initialBlockKzg: KzgCommitment,
    // derived
    config: Tx.Builder.Config,
    // in head config
    txTiming: TxTiming,
    // in head config
    tallyFeeAllowance: Coin,
    // in head config
    equityShares: EquityShares,
    // derived from init tx (which is in the config)
    multisigRegimeUtxo: MultisigRegimeUtxo,
    // in head config (move into TxTiming)
    votingDuration: FiniteDuration,
    // derived from init tx (which is in the config)
    treasuryTokenName: AssetName,
    // derived from init tx (which is in the config)
    initialTreasury: MultisigTreasuryUtxo
) extends Actor[IO, Requests.Request] {

    val state: Ref[IO, JointLedger.State] =
        Ref.unsafe[IO, JointLedger.State](
          Done(
            producedBlock = Block.Initial(
              Block.Header.Initial(timeCreation = initialBlockTime, commitment = initialBlockKzg)
            ),
            lastFallbackValidityStart = None,
            dappLedgerState = DappLedgerM.State(initialTreasury, Queue.empty),
            virtualLedgerState = VirtualLedgerM.State.empty
          )
        )

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
              action = DappLedgerM.registerDeposit(serializedDeposit, eventId, virtualOutputs),
              // Left == deposit rejected
              // FIXME: This should probably be returned as  sum type in the Right
              onFailure = _ =>
                  for {
                      oldState <- unsafeGetProducing
                      newState = oldState
                          .focus(_.nextBlockData.events)
                          .modify(_.appended((eventId, false)))
                      _ <- state.set(newState)
                  } yield (),
              onSuccess = _ =>
                  for {
                      oldState <- unsafeGetProducing
                      newState = oldState
                          .focus(_.nextBlockData.events)
                          .modify(_.appended((eventId, true)))
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
                          .modify(_.appended((eventId, true)))
                      _ <- state.set(newState)
                  } yield (),
              // Valid transaction continuation
              onSuccess = payoutObligations =>
                  for {
                      p <- unsafeGetProducing
                      newState = p
                          .focus(_.nextBlockData.events)
                          .modify(_.appended((eventId, false)))
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
          BlockEffects.Minor(nextBlock.id, List.empty)
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
                      // FIXME: This is not currently the CORRECT commitment. See the comment in DappLedger regarding
                      // calling out to the virtual ledger
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
                      config.tokenNames.headTokenName.bytes ++
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
                    tallyFeeAllowance = this.tallyFeeAllowance,
                    votingDuration = this.votingDuration,
                    immatureDeposits = immatureDeposits,
                    blockCreatedOn = blockCreatedOn,
                    competingFallbackValidityStart = blockCreatedOn
                        + txTiming.minSettlementDuration
                        + txTiming.inactivityMarginDuration
                        + txTiming.silenceDuration,
                    txTiming = txTiming
                  ),
                  onSuccess = IO.pure
                )

                // Is it safe to apply this now?
                _ <- this.runVirtualLedgerM(VirtualLedgerM.applyGenesisEvent(genesisEvent))
            } yield settleLedgerRes

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
                        java.time.Instant.ofEpochMilli(
                          deposit._2.datum.refundInstructions.startTime.toLong
                        ) - txTiming.silenceDuration
                    val depositAbsorptionStart: java.time.Instant =
                        depositValidityEnd + txTiming.depositMaturityDuration
                    val depositAbsorptionEnd: java.time.Instant =
                        depositAbsorptionStart + txTiming.depositAbsorptionDuration
                    // FIXME: This is partial and will probably crash on the initial block
                    val settlementValidityEnd: Option[java.time.Instant] =
                        producing.competingFallbackValidityStart.map(_ - txTiming.silenceDuration)
                    {
                        if depositAbsorptionStart.isAfter(producing.startTime)
                        // Not yet mature
                        then acc.focus(_._1).modify(_.appended(deposit))
                        else if pollResults.contains(UtxoIdL1(deposit._2.toUtxo.input)) &&
                        (depositAbsorptionStart.isBefore(
                          producing.startTime
                        ) || depositAbsorptionStart == producing.startTime) &&
                        (settlementValidityEnd.isEmpty || settlementValidityEnd.get.isBefore(
                          depositAbsorptionEnd
                        ) || settlementValidityEnd.get == depositAbsorptionEnd)
                        // Eligible for absorption
                        then acc.focus(_._2).modify(_.appended(deposit))
                        else if ((depositAbsorptionStart.isBefore(
                          producing.startTime
                        ) || depositAbsorptionStart == producing.startTime)
                            && !pollResults.contains(UtxoIdL1(deposit._2.toUtxo.input))) ||
                        (settlementValidityEnd.isEmpty || settlementValidityEnd.get
                            .isAfter(depositAbsorptionEnd))
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
                if producing.competingFallbackValidityStart.isEmpty
                then true
                else
                    txTiming.minSettlementDuration.toMillis >=
                        // FIXME: This time handling is a bit wonky because of the java <> scala mix
                        producing.competingFallbackValidityStart.get.toEpochMilli
                        - txTiming.silenceDuration.toMillis
                        - producing.startTime.toEpochMilli

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
            _ <- sendAugmentedBlock(augmentedBlock)
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
        for {
            p <- unsafeGetProducing

            finalizationTxSeq <- this.runDappLedgerM(
              DappLedgerM.finalizeLedger(
                payoutObligationsRemaining = p.nextBlockData.blockWithdrawnUtxos,
                multisigRegimeUtxoToSpend = multisigRegimeUtxo,
                equityShares = equityShares,
                blockCreatedOn = p.startTime,
                competingFallbackValidityStart = p.startTime
                    + txTiming.minSettlementDuration
                    + txTiming.inactivityMarginDuration
                    + txTiming.silenceDuration,
                txTiming = txTiming
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
            _ <- sendAugmentedBlock(augmentedBlock)

        } yield ()
    }

    // when a block is finished, we:
    //   - send Aug block to block signer along with the L1 effects settlementTxSeq, etc. for signing locally
    //     (signatures subsequently passed to peer liaison for circulation and block weaver and to the )
    //   - sends block itself to peer liaison for circulation
    //   - sends just L1 effects to cardano liaison
    private def sendAugmentedBlock(augmentedBlock: AugmentedBlock.Next): IO[Unit] =
        for {
            // _ <- blockSigner ! augmentedBlock
            _ <- IO.parSequence(peerLiaisons.map(_ ! augmentedBlock.blockNext))
        } yield ()

    private def checkReferenceBlock(
        expectedBlock: Option[Block],
        actualBlock: AugmentedBlock
    ): IO[Unit] = for {
        p <- unsafeGetProducing
        fallbackValidityStart: Option[java.time.Instant] =
            actualBlock match {
                case major: types.AugmentedBlock.Major =>
                    Some(major.effects.fallback.validityStart)
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

    final case class CompleteBlockError() extends Throwable

    object Requests {
        type Request =
            // RegisterDeposit is exactly the DappLedger type, we're simply forwarding it through.
            // Does this mean we should wrap it?
            LedgerEvent | StartBlock | CompleteBlockRegular | CompleteBlockFinal | GetState.Sync

        case class StartBlock(
            blockNum: Block.Number,
            blockCreationTime: Instant
        )

        /** @param referenceBlock
          * @param pollResults
          *   there are two reasons to have it here:
          *   - pollResults are always absent upon weaver's start time. Passing it here may improve
          *     things.
          *   - pollResults are needed only when we are finishing a regular (non-final) block.
          */
        case class CompleteBlockRegular(
            referenceBlock: Option[Block],
            pollResults: Set[UtxoIdL1]
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
        lastFallbackValidityStart: Option[java.time.Instant],
        override val dappLedgerState: DappLedgerM.State,
        override val virtualLedgerState: VirtualLedgerM.State
    ) extends State

    final case class Producing(
        override val dappLedgerState: DappLedgerM.State,
        override val virtualLedgerState: VirtualLedgerM.State,
        previousBlock: Block,
        // None for the first block
        competingFallbackValidityStart: Option[java.time.Instant],
        startTime: java.time.Instant,
        nextBlockData: TransientFields
    ) extends State
}
