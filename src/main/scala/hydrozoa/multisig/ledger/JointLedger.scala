package hydrozoa.multisig.ledger

import cats.data.EitherT
import cats.effect.{IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.EquityShares
import hydrozoa.multisig.ledger.DappLedger.Requests.{RegisterDeposit, SettleLedger}
import hydrozoa.multisig.ledger.JointLedger.*
import hydrozoa.multisig.ledger.JointLedger.Requests.{ApplyInternalTxL2, CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import hydrozoa.multisig.ledger.VirtualLedger.{ApplyInternalTx, ErrorApplyInternalTx}
import hydrozoa.multisig.ledger.dapp.tx.RolloutTx
import hydrozoa.multisig.ledger.dapp.txseq.SettlementTxSeq.{NoRollouts, WithRollouts}
import hydrozoa.multisig.ledger.dapp.txseq.{FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.dapp.utxo.MultisigRegimeUtxo
import hydrozoa.multisig.ledger.joint.utxo.Payout
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.protocol.ConsensusProtocol
import hydrozoa.multisig.protocol.types.*
import hydrozoa.multisig.protocol.types.Block.*
import java.util.concurrent.TimeUnit
import monocle.syntax.all.*
import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scalus.cardano.ledger.{AssetName, Coin}
import scalus.ledger.api.v3.PosixTime

private case class TransientFields(
    ledgerEventsRequired: Map[Peer.Number, LedgerEvent.Number],
    transactionsValid: List[LedgerEvent.Id],
    transactionsInvalid: List[LedgerEvent.Id],
    depositsRegistered: List[LedgerEvent.Id],
    depositsRejected: List[LedgerEvent.Id],
    blockWithdrawnUtxos: Vector[Payout.Obligation.L1]
)

sealed trait State

final case class Done(producedBlock: Block) extends State

final case class Producing(
    previousBlock: Block,
    startTime: PosixTime,
    nextBlockData: TransientFields
) extends State

// NOTE: Joint ledger is created by the MultisigManager.
// NOTE: As of 2025-11-16, George says BlockWeaver should be the ONLY actor calling the joint ledger
final case class JointLedger(
    private val dappLedger: ActorRef[IO, DappLedger.Requests.Request],
    private val virtualLedger: ActorRef[IO, VirtualLedger.Request],
    private val peerLiaisons: Seq[ActorRef[IO, ConsensusProtocol.PeerLiaison.Request]],
    private val state: Ref[IO, State],
    private val tallyFeeAllowance: Coin,
    private val equityShares: EquityShares,
    private val multisigRegimeUtxo: MultisigRegimeUtxo,
    private val votingDuration: PosixTime,
    private val treasuryTokenName: AssetName
) extends Actor[IO, Requests.Request] {

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
        // NOTE: we don't call d.handleRequest here, because the DappLedger will fill the deferred,
        // NOT the joint ledger!
        case d: RegisterDeposit      => registerDeposit(d)
        case a: ApplyInternalTxL2    => applyInternalTxL2(a)
        case s: StartBlock           => startBlock(s)
        case c: CompleteBlockRegular => completeBlockRegular(c)
        case f: CompleteBlockFinal   => completeBlockFinal(f)
    }

    /** Update the JointLedger's state -- the work-in-progress block -- to accept or reject deposits
      * depending on whether the [[dappLedger]] Actor can successfully register the deposit,
      */
    private def registerDeposit(req: DappLedger.Requests.RegisterDeposit): IO[Unit] = {
        // Given an old state and a rejected eventId, add the event ID to depositsRejected and update
        // ledgerEventsRequired.
        //
        // NOTE: This assumes that the ledger events will always be passed in per-peer order. How can we establish
        // this invariant conclusively?
        def rejectDeposit(oldState: Producing, eventId: LedgerEvent.Id): IO[Unit] = {
            val newState = oldState
                .focus(_.nextBlockData.depositsRejected)
                .modify(_.appended(eventId))
                .focus(_.nextBlockData.ledgerEventsRequired)
                .modify((m: Map[Peer.Number, LedgerEvent.Number]) =>
                    m.updated(eventId.peerNum, eventId.eventNum)
                )
            state.set(newState)
        }

        // Given an old state and a registered eventId, add the event ID to depositsRegistered and update
        // ledgerEventsRequired
        def registerDepositInState(oldState: Producing, eventId: LedgerEvent.Id): IO[Unit] = {
            val newState = oldState
                .focus(_.nextBlockData.depositsRegistered)
                .modify(_.appended(eventId))
                .focus(_.nextBlockData.ledgerEventsRequired)
                .modify((m: Map[Peer.Number, LedgerEvent.Number]) =>
                    m.updated(eventId.peerNum, eventId.eventNum)
                )
            state.set(newState)
        }

        // NOTE: as currently implemented, this will block until the dappLedger is finished parsing the deposit.
        // Is this what we want, or do we want the dappLedger to notify the joint ledger async instead?
        for {
            s <- unsafeGetProducing
            eRes <- dappLedger ?: req
            _ <- eRes match {
                // Rejected deposit
                case Left(e)  => rejectDeposit(s, req.eventId)
                case Right(d) => registerDepositInState(s, req.eventId)
            }
        } yield ()
    }

    /** Update the current block with the result of passing the tx to the virtual ledger, as well as
      * updating ledgerEventsRequired
      */
    private def applyInternalTxL2(
        args: ApplyInternalTxL2
    ): IO[Either[ErrorApplyInternalTx, Unit]] = {
        import args.*
        def appendTransactionsValid(
            oldState: Producing,
            eventId: LedgerEvent.Id,
            payouts: Vector[Payout.Obligation.L1]
        ): IO[Unit] =
            val newState = oldState
                .focus(_.nextBlockData.transactionsValid)
                .modify(_.appended(id))
                .focus(_.nextBlockData.ledgerEventsRequired)
                .modify(m => m.updated(id.peerNum, id.eventNum))
                .focus(_.nextBlockData.blockWithdrawnUtxos)
                .modify(v => v ++ payouts)
            state.set(newState)

        def appendTransactionsInvalid(oldState: Producing, eventId: LedgerEvent.Id): IO[Unit] =
            val newState = oldState
                .focus(_.nextBlockData.transactionsInvalid)
                .modify(_.appended(id))
                .focus(_.nextBlockData.ledgerEventsRequired)
                .modify(m => m.updated(id.peerNum, id.eventNum))
            state.set(newState)

        val eitherT: EitherT[IO, ErrorApplyInternalTx, Unit] = for {
            req <- EitherT.right(ApplyInternalTx(tx))
            res <- EitherT.right(virtualLedger ?: req)
            p <- EitherT.right(unsafeGetProducing)
            newState = res match {
                case Left(_)        => appendTransactionsInvalid(p, id)
                case Right(payouts) => appendTransactionsValid(p, id, payouts)
            }
            _ <- EitherT.right(state.set(p))
        } yield ()
        eitherT.value
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
                d.producedBlock,
                blockCreationTime,
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
            settleLedgerRes: SettleLedger.ResultWithoutSettlement
        ): IO[AugmentedBlock.Minor] = {
            import p.nextBlockData.*
            val nextBlockBody: Block.Body.Minor = Block.Body.Minor(
              ledgerEventsRequired = ledgerEventsRequired,
              transactionsValid = transactionsValid,
              transactionsInvalid = transactionsInvalid,
              depositsRegistered = depositsRegistered,
              depositsRejected = depositsRejected,
              depositsRefunded = settleLedgerRes.refundedDeposits.toList.map(_._1)
            )

            for {
                gsReq <- VirtualLedger.GetState()
                gsRes <- virtualLedger ?: gsReq

                vrState = gsRes.getOrElse(
                  throw new RuntimeException("error getting state from virtual ledger")
                )

                kzgCommit = KzgCommitment.hashToScalar(vrState.activeUtxos)

                // FIXME: unsafe cast
                nextBlock: Block.Minor = p.previousBlock
                    .nextBlock(
                      newBody = nextBlockBody,
                      // FIXME: Conflicting types
                      newTime = FiniteDuration(p.startTime.toLong, SECONDS),
                      // FIXME: Conflicting types
                      newCommitment = KzgCommitment.calculateCommitment(kzgCommit)
                    )
                    .asInstanceOf[Block.Minor]
            } yield
                // TODO: Not handling refunds right now
                AugmentedBlock.Minor(
                  nextBlock,
                  BlockEffects.Minor(nextBlock.id, List.empty, List.empty)
                )

        }
        def augmentedBlockMajor(
            p: Producing,
            settleLedgerRes: SettleLedger.ResultWithSettlement
        ): AugmentedBlock.Major = {
            import p.nextBlockData.*
            val nextBlockBody: Block.Body.Major = Block.Body.Major(
              ledgerEventsRequired = ledgerEventsRequired,
              transactionsValid = transactionsValid,
              transactionsInvalid = transactionsInvalid,
              depositsRegistered = depositsRegistered,
              depositsRejected = depositsRejected,
              depositsAbsorbed = settleLedgerRes.absorbedDeposits.map(_._1).toList,
              depositsRefunded = settleLedgerRes.refundedDeposits.map(_._1).toList
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

        for {
            producing <- unsafeGetProducing

            settleLedgerReq <- SettleLedger(
              pollDepositResults = pollResults,
              payouts = producing.nextBlockData.blockWithdrawnUtxos,
              blockCreationTime = producing.startTime,
              tallyFeeAllowance = tallyFeeAllowance,
              votingDuration = votingDuration
            )

            settleLedgerRes <- (dappLedger ?: settleLedgerReq).map {
                // should this error be thrown here or handled in the DappLedger?
                case Left(e)  => throw new RuntimeException(s"could not settle DappLedger. $e")
                case Right(r) => r
            }

            // This is applying the effect to the L2 ledger before we actually spend the deposits on L1.
            // I think there could be a race here?
            augmentedBlock <- settleLedgerRes match {
                case r: SettleLedger.ResultWithoutSettlement => augmentBlockMinor(producing, r)
                case r: SettleLedger.ResultWithSettlement =>
                    IO.pure(augmentedBlockMajor(producing, r))
            }

            _ <- checkReferenceBlock(referenceBlock, augmentedBlock.block)
            _ <- sendAugmentedBlock(augmentedBlock)
        } yield ()
    }

    // Block completion Signal is provided to the joint ledger when the block weaver says its time.
    // If its a final block, we don't pass poll results from the cardano liason. Otherwise we do.
    // We need to:
    //   - Compile the information from the transient fields into a block
    //   - put it into "previous block"
    //   - wipe the "transient fields"
    // If a "reference block" is passed, this means that the block we produce must be equal to the reference block.
    // If the produced block is NOT equal to a passed reference block, then:
    //   - Consensus is broken
    //   - Send a panic to the multisig regime manager in a suicide note

    // TODO: call settle ledger
    def completeBlockFinal(args: CompleteBlockFinal): IO[Unit] = {
        import args.*
        for {
            p <- unsafeGetProducing

            finalizeLedgerReq <- DappLedger.Requests.FinalizeLedger(
              p.nextBlockData.blockWithdrawnUtxos,
              multisigRegimeUtxoToSpend = multisigRegimeUtxo,
              equityShares = equityShares
            )

            finalizeLedgerRes <- (dappLedger ?: finalizeLedgerReq).map {
                case Left(e)  => throw new RuntimeException("Could not finalize ledger")
                case Right(r) => r
            }

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
                    val rollouts: List[RolloutTx] = finalizeLedgerRes match {
                        case _: Monolithic => List.empty
                        case _: WithDeinit => List.empty
                        case x: FinalizationTxSeq.WithRollouts =>
                            x.rolloutTxSeq.notLast.appended(x.rolloutTxSeq.last).toList
                        case x: WithDeinitAndRollouts =>
                            x.rolloutTxSeq.notLast.appended(x.rolloutTxSeq.last).toList
                    }
                    BlockEffects.Final(
                      nextBlock.id,
                      finalizeLedgerRes.finalizationTx,
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
    //     (signatures subsequently passed to peer liason for circulation and block weaver and to the )
    //   - sends block itself to peer liason for circulation
    //   - sends just L1 effects to cardano liason
    private def sendAugmentedBlock(augmentedBlock: AugmentedBlock.Next): IO[Unit] =
        for {
            // _ <- blockSigner ! augmentedBlock
            _ <- IO.parSequence(peerLiaisons.map(_ ! augmentedBlock.block))
            // _ <- cardanoLiason ! augementedBlock._2
        } yield ()

    private def checkReferenceBlock(expectedBlock: Option[Block], actualBlock: Block): IO[Unit] =
        expectedBlock match {
            case Some(refBlock) if refBlock == actualBlock => state.set(Done(actualBlock))
            case Some(_) =>
                panic(
                  "Reference block didn't match actual block; consensus is broken."
                ) >> context.self.stop
            case None => state.set(Done(actualBlock))
        }

    // Sends a panic to the multisig regime manager, indicating that the node can proceed any more
    // FIXME: implement
    private def panic(msg: String): IO[Unit] = IO.pure(())
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
                CompleteBlockFinal

        case class ApplyInternalTxL2(id: LedgerEvent.Id, tx: Array[Byte])

        case class StartBlock(blockCreationTime: PosixTime)

        case class CompleteBlockRegular(
            pollResults: Set[LedgerEvent.Id],
            referenceBlock: Option[Block]
        )

        case class CompleteBlockFinal(referenceBlock: Option[Block])
    }
}
