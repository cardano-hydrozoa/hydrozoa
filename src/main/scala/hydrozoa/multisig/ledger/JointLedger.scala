package hydrozoa.multisig.ledger

import cats.data.*
import cats.effect.{IO, Ref}
import hydrozoa.multisig.ledger.dapp.tx.Tx
import com.suprnation.actor.Actor.{Actor, Receive}

import scala.collection.immutable.Queue
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.EquityShares
import hydrozoa.lib.actor.*
import hydrozoa.multisig.ledger.DappLedgerM.{runDappLedgerM, settleLedger}
import hydrozoa.multisig.ledger.JointLedger.*
import hydrozoa.multisig.ledger.JointLedger.Requests.{ApplyInternalTxL2, CompleteBlockFinal, CompleteBlockRegular, GetState, StartBlock}
import hydrozoa.multisig.ledger.VirtualLedgerM.runVirtualLedgerM
import hydrozoa.multisig.ledger.dapp.tx.RolloutTx
import hydrozoa.multisig.ledger.dapp.txseq.DepositRefundTxSeq.ParseError.VirtualOutputs
import hydrozoa.multisig.ledger.dapp.txseq.SettlementTxSeq.{NoRollouts, WithRollouts}
import hydrozoa.multisig.ledger.dapp.txseq.{FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.{GenesisObligation, L2EventGenesis}
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.protocol.ConsensusProtocol
import hydrozoa.multisig.protocol.types.*
import hydrozoa.multisig.protocol.types.Block.*
import hydrozoa.multisig.protocol.types.Block.Version.Full

import java.util.concurrent.TimeUnit
import monocle.syntax.all.*
import scalus.builtin.{ByteString, platform}

import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scalus.cardano.ledger.{AssetName, Coin, TransactionHash}
import scalus.ledger.api.v3.PosixTime

// Fields of a work-in-progress block, with an additional field for dealing with withdrawn utxos
private case class TransientFields(
    ledgerEventsRequired: Map[Peer.Number, LedgerEvent.Number],
    transactionsValid: List[LedgerEvent.Id],
    transactionsInvalid: List[LedgerEvent.Id],
    depositsRegistered: List[LedgerEvent.Id],
    depositsRejected: List[LedgerEvent.Id],
    blockWithdrawnUtxos: Vector[Payout.Obligation]
)

// NOTE: Joint ledger is created by the MultisigManager.
// NOTE: As of 2025-11-16, George says BlockWeaver should be the ONLY actor calling the joint ledger
final case class JointLedger(
    peerLiaisons: Seq[ActorRef[IO, ConsensusProtocol.PeerLiaison.Request]],
    // private val cardanoLiaison
    // private val blockSigner
    initialBlockTime: FiniteDuration,
    initialBlockKzg: KzgCommitment,
    //// Static config fields
    config : Tx.Builder.Config,
    tallyFeeAllowance: Coin,
    equityShares: EquityShares,
    multisigRegimeUtxo: MultisigRegimeUtxo,
    votingDuration: PosixTime,
    treasuryTokenName: AssetName,
    initialTreasury : MultisigTreasuryUtxo
) extends Actor[IO, Requests.Request] {


    val state: Ref[IO, JointLedger.State] =
      Ref.unsafe[IO, JointLedger.State](
        Done(
          producedBlock = Block.Initial(
            Block.Header.Initial(timeCreation = initialBlockTime, commitment = initialBlockKzg)
          ),
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

    override def receive: Receive[IO, Requests.Request] = PartialFunction.fromFunction {
        case d : JointLedger.Requests.RegisterDeposit => registerDeposit(d)
        case a: ApplyInternalTxL2    => applyInternalTxL2(a)
        case s: StartBlock           => startBlock(s)
        case c: CompleteBlockRegular => completeBlockRegular(c)
        case f: CompleteBlockFinal   => completeBlockFinal(f)
        case req: SyncRequest.Any =>
            req.request match {
              case r: GetState.type => r.handleSync(req, _ => state.get)
            }

    }

    /** Update the JointLedger's state -- the work-in-progress block -- to accept or reject deposits
      * depending on whether the [[dappLedger]] Actor can successfully register the deposit,
      */
    private def registerDeposit(req : JointLedger.Requests.RegisterDeposit): 
        IO[Unit] = {
        import req.*
        for {
            oldState <- unsafeGetProducing
            _ <- this.runDappLedgerM(
              action = DappLedgerM.registerDeposit(serializedDeposit, eventId, virtualOutputs),
              // Left == deposit rejected
              // FIXME: This should probably be returned as  sum type in the Right
              onFailure = _ => {
                val newState = oldState
                  .focus(_.nextBlockData.depositsRejected).modify(_.appended(eventId))
                  .focus(_.nextBlockData.ledgerEventsRequired)
                  .modify((m: Map[Peer.Number, LedgerEvent.Number]) =>
                    m.updated(eventId.peerNum, eventId.eventNum)
                  )
                state.set(newState)
              },
              onSuccess = _ => {
                val newState = oldState
                  .focus(_.nextBlockData.depositsRegistered).modify(_.appended(eventId))
                  .focus(_.nextBlockData.ledgerEventsRequired)
                  .modify((m: Map[Peer.Number, LedgerEvent.Number]) =>
                    m.updated(eventId.peerNum, eventId.eventNum))
                state.set(newState)
              }
            )
        } yield ()
    }

    /** Update the current block with the result of passing the tx to the virtual ledger, as well as
      * updating ledgerEventsRequired
      */
    private def applyInternalTxL2(
        args: ApplyInternalTxL2
    ): IO[Unit] = {
        import args.*

        for {
            p <- unsafeGetProducing
            _ <- this.runVirtualLedgerM (
              action = VirtualLedgerM.applyInternalTx(tx),
              // Invalid transaction continuation
              onFailure = _ => {
                val newState = p
                  .focus(_.nextBlockData.transactionsInvalid)
                  .modify(_.appended(id))
                  .focus(_.nextBlockData.ledgerEventsRequired)
                  .modify(m => m.updated(id.peerNum, id.eventNum))
                state.set(newState)
              },
              // Valid transaction continuation
              onSuccess = payoutObligations => {
                val newState = p
                  .focus(_.nextBlockData.transactionsValid)
                  .modify(_.appended(id))
                  .focus(_.nextBlockData.ledgerEventsRequired)
                  .modify(m => m.updated(id.peerNum, id.eventNum))
                  .focus(_.nextBlockData.blockWithdrawnUtxos)
                  .modify(v => v ++ payoutObligations)
                state.set(newState)
              })
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
                startTime = blockCreationTime,
                pollResults = args.pollResults,
                TransientFields(
                  ledgerEventsRequired = d.producedBlock match {
                      case i: Initial   => Map.empty
                      case minor: Minor => minor.body.ledgerEventsRequired
                      case major: Major => major.body.ledgerEventsRequired
                      // TODO: type better
                      case f: Final =>
                          throw new RuntimeException(
                            "JointLedger called startBlock when the previous" +
                                " block was a final block"
                          )
                  },
                  transactionsValid = List.empty,
                  transactionsInvalid = List.empty,
                  depositsRegistered = List.empty,
                  depositsRejected = List.empty,
                  blockWithdrawnUtxos = Vector.empty
                )
                , dappLedgerState = d.dappLedgerState
                , virtualLedgerState = d.virtualLedgerState
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
            p: Producing,
            depositsRefunded : List[LedgerEvent.Id]
        ): AugmentedBlock.Minor = {
            import p.nextBlockData.*
            val nextBlockBody: Block.Body.Minor = Block.Body.Minor(
              ledgerEventsRequired = ledgerEventsRequired,
              transactionsValid = transactionsValid,
              transactionsInvalid = transactionsInvalid,
              depositsRegistered = depositsRegistered,
              depositsRejected = depositsRejected,
              depositsRefunded = depositsRefunded
            )

            val kzgCommit = p.virtualLedgerState.kzgCommitment
            val nextBlock: Block.Minor = p.previousBlock
                .nextBlock(
                    newBody = nextBlockBody,
                    // FIXME: Conflicting types
                    newTime = FiniteDuration(p.startTime.toLong, SECONDS),
                    // FIXME: Conflicting types
                    newCommitment = kzgCommit
                )
                .asInstanceOf[Block.Minor]
         
            AugmentedBlock.Minor(
                  nextBlock,
                  BlockEffects.Minor(nextBlock.id, List.empty, List.empty)
                )

        }
        def augmentedBlockMajor(
            p: Producing,
            settleLedgerRes: DappLedgerM.SettleLedger.Result,
            refundedDeposits : List[LedgerEvent.Id],
            absorbedDeposits : List[LedgerEvent.Id]
        ): AugmentedBlock.Major = {
            import p.nextBlockData.*
            val nextBlockBody: Block.Body.Major = Block.Body.Major(
              ledgerEventsRequired = ledgerEventsRequired,
              transactionsValid = transactionsValid,
              transactionsInvalid = transactionsInvalid,
              depositsRegistered = depositsRegistered,
              depositsRejected = depositsRejected,
              depositsAbsorbed = absorbedDeposits,
              depositsRefunded = refundedDeposits
            )

            // FIXME: unsafe cast
            val nextBlock: Block.Major = p.previousBlock
                .nextBlock(
                  newBody = nextBlockBody,
                  newTime = FiniteDuration(p.startTime.toLong, TimeUnit.SECONDS),
                  // FIXME: This is not currently the CORRECT commitment. See the comment in DappLedger regarding
                  // calling out to the virtual ledger
                  newCommitment = IArray.unsafeFromArray(
                    settleLedgerRes.settlementTxSeq.settlementTx.treasuryProduced.datum.commit.bytes
                  )
                )
                .asInstanceOf[Block.Major]

            AugmentedBlock.Major(
              nextBlock,
              BlockEffects.Major(
                nextBlock.id,
                settlement = settleLedgerRes.settlementTxSeq.settlementTx,
                rollouts = settleLedgerRes.settlementTxSeq match {
                    case _: NoRollouts => List.empty
                    case r: WithRollouts =>
                        r.rolloutTxSeq.notLast.appended(r.rolloutTxSeq.last).toList
                },
                fallback = settleLedgerRes.fallBack,
                immediateRefunds = List.empty,
                postDatedRefunds = List.empty
              )
            )
        }

        // FIXME: placeholder
        def isMature(deposit : DepositUtxo) : Boolean = true


        def doSettlement(validDeposits : NonEmptyList[(LedgerEvent.Id, DepositUtxo)]
                        , treasuryToSpend: MultisigTreasuryUtxo
                        , payoutObligations: Vector[Payout.Obligation]
                        , immatureDeposits : Queue[(LedgerEvent.Id, DepositUtxo)]) : IO[DappLedgerM.SettleLedger.Result] = {
            val genesisObligations : NonEmptyList[GenesisObligation] = ???
            val genesisEvent = L2EventGenesis(
                genesisObligations,
              TransactionHash.fromByteString(platform.blake2b_256(config.tokenNames.headTokenName.bytes ++
                               ByteString.fromBigIntBigEndian(BigInt(treasuryToSpend.datum.versionMajor.toInt + 1))))
            )
            for {
              nextKzg <- this.runVirtualLedgerM(VirtualLedgerM.mockApplyGenesis(genesisEvent))
              
              settleLedgerRes <- this.runDappLedgerM(DappLedgerM.settleLedger(
                nextKzg = nextKzg, 
                validDeposits = validDeposits, 
                payoutObligations = payoutObligations, 
                tallyFeeAllowance = this.tallyFeeAllowance,
                votingDuration = this.votingDuration,
                immatureDeposits = immatureDeposits
              ),
                onSuccess = IO.pure)
              
              // Is it safe to apply this now?
              _ <- this.runVirtualLedgerM(VirtualLedgerM.applyGenesisEvent(genesisEvent))
            } yield settleLedgerRes
        }

        for {
            producing <- unsafeGetProducing
            dappLedgerState : DappLedgerM.State <- this.runDappLedgerM(DappLedgerM.get, onSuccess = IO.pure)

            //===================================
            // Step 1: Figure out which deposits are valid and turn them into genesis obligations
            // ===================================

            // TODO: partitioning probably isn't the fastest way, because it will inspect each
            // element of the queue. But I don't recall if we assume the queue is sorted according to
            // maturity time, so I'll go with this for now. If it is sorted, there's almost certainly
            // a more efficient function.
            depositsPartition = dappLedgerState.deposits.partition(x => isMature(x._2))
            matureDeposits = depositsPartition._1
            immatureDeposits = depositsPartition._2

            // Tuple containing (depositsInPollResults, depositsNotInPollResults)
            depositPartition = matureDeposits.partition(x => producing.pollResults.contains(x._1))
            depositsInPollResults = depositPartition._1

            // TODO: these just get ignored for now. In the future, we'd want to create a RefundImmediate
            depositsNotInPollResults = depositPartition._2

            isMinorBlock : Boolean =
              depositsInPollResults.isEmpty && producing.nextBlockData.blockWithdrawnUtxos.isEmpty

            augmentedBlock <- if isMinorBlock 
                 then IO.pure(augmentBlockMinor(producing, depositsRefunded = depositsNotInPollResults.toList.map(_._1)))
            else {
              for {
                  settlementRes <- doSettlement(
                    validDeposits = ???, //depositsInPollResults,
                    treasuryToSpend = ???,
                    payoutObligations = ???,
                    immatureDeposits = ???
                  )
              } yield augmentedBlockMajor(producing, ???, ???, ???)
            } 
            
            _ <- checkReferenceBlock(referenceBlock, augmentedBlock.block)
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

            finalizationTxSeq <- this.runDappLedgerM(DappLedgerM.finalizeLedger(
                payoutObligationsRemaining = p.nextBlockData.blockWithdrawnUtxos,
                multisigRegimeUtxoToSpend = multisigRegimeUtxo,
                equityShares = equityShares),
              onSuccess = IO.pure)

            augmentedBlock: AugmentedBlock.Final = {
                import p.nextBlockData.*
                val nextBlockBody: Block.Body.Final = Block.Body.Final(
                  ledgerEventsRequired = ledgerEventsRequired,
                  transactionsValid = transactionsValid,
                  transactionsInvalid = transactionsInvalid,
                  depositsRejected = depositsRejected ++ depositsRegistered,
                  depositsRefunded = List.empty // FIXME: currently not handling refunds
                )

                // FIXME: unsafe cast
                val nextBlock: Block.Final = p.previousBlock
                    .nextBlock(
                      nextBlockBody,
                      FiniteDuration(p.startTime.toLong, TimeUnit.SECONDS),
                      IArray.empty[Byte]
                    )
                    .asInstanceOf[Block.Final]

                val blockEffects: BlockEffects.Final = {
                    import FinalizationTxSeq.*
                    val rollouts: List[RolloutTx] = finalizationTxSeq match {
                        case _: Monolithic => List.empty
                        case _: WithDeinit => List.empty
                        case x: FinalizationTxSeq.WithRollouts =>
                            x.rolloutTxSeq.notLast.appended(x.rolloutTxSeq.last).toList
                        case x: WithDeinitAndRollouts =>
                            x.rolloutTxSeq.notLast.appended(x.rolloutTxSeq.last).toList
                    }
                    BlockEffects.Final(
                      nextBlock.id,
                      finalizationTxSeq.finalizationTx,
                      rollouts = rollouts,
                      immediateRefunds = List.empty
                    )
                }

                AugmentedBlock.Final(nextBlock, blockEffects)
            }

            _ <- checkReferenceBlock(referenceBlock, augmentedBlock.block)
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
            _ <- IO.parSequence(peerLiaisons.map(_ ! augmentedBlock.block))
            // _ <- cardanoLiaison ! augmentedBlock._2
        } yield ()

    private def checkReferenceBlock(expectedBlock: Option[Block], actualBlock: Block): IO[Unit] =
        expectedBlock match {
            case Some(refBlock) if refBlock == actualBlock => state.update(s =>
              Done(actualBlock, s.dappLedgerState, s.virtualLedgerState))
            case Some(_) =>
                panic(
                  "Reference block didn't match actual block; consensus is broken."
                ) >> context.self.stop
            case None => state.update(s => Done(actualBlock, s.dappLedgerState, s.virtualLedgerState))
        }

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



    final case class CompleteBlockError() extends Throwable
    object Requests {
        type Request =
            // RegisterDeposit is exactly the DappLedger type, we're simply forwarding it through.
            // Does this mean we should wrap it?
            RegisterDeposit | ApplyInternalTxL2 | StartBlock | CompleteBlockRegular |
                CompleteBlockFinal | GetState.Sync

        // FIXME: This should include the refundTxBytes
        // FIXME: The virtual outputs should not be parsed yet (i.e. Array[Byte])
        final case class RegisterDeposit(
                                        serializedDeposit : Array[Byte],
                                         eventId : LedgerEvent.Id, virtualOutputs : NonEmptyList[GenesisObligation]
        )

        case class ApplyInternalTxL2(id: LedgerEvent.Id, tx: Array[Byte])

        // FIXME: Make this take a pollResults: Utxos of all utxos present at the treasury address
        case class StartBlock(blockCreationTime: PosixTime, pollResults: Set[LedgerEvent.Id])

        case class CompleteBlockRegular(
            referenceBlock: Option[Block],
            // Make this block Major in order to circumvent a fallback tx becoming valid
            // forceMajor : Boolean
        )

        case class CompleteBlockFinal(referenceBlock: Option[Block])

        case object GetState extends SyncRequest[IO, GetState.type, State] {
            type Sync = SyncRequest.Envelope[IO, GetState.type, State]

            def ?: : this.Send = SyncRequest.send(_, this)
        }
    }

    sealed trait State{
      val dappLedgerState: DappLedgerM.State
      val virtualLedgerState : VirtualLedgerM.State
    }

    final case class Done(producedBlock: Block,
                          override val dappLedgerState: DappLedgerM.State,
                          override val virtualLedgerState: VirtualLedgerM.State) extends State

    final case class Producing(
                                override val dappLedgerState: DappLedgerM.State,
                                override val virtualLedgerState : VirtualLedgerM.State,
                                previousBlock: Block,
                                startTime: PosixTime,
                                pollResults: Set[LedgerEvent.Id],
                                nextBlockData: TransientFields
    ) extends State
}
