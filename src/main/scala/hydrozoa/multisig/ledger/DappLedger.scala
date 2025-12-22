package hydrozoa.multisig.ledger

import cats.data.{EitherT, NonEmptyList}
import cats.effect.IO.realTime
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.actor.ReplyingActor
import hydrozoa.config.EquityShares
import hydrozoa.lib.actor.{SyncRequest, SyncRequestE}
import hydrozoa.multisig.ledger.DappLedger.Errors.*
import hydrozoa.multisig.ledger.DappLedger.Requests.*
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.txseq.{FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.GenesisObligation
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.protocol.types.{Block, LedgerEvent}
import scala.collection.immutable.Queue
import scala.language.implicitConversions
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.ledger.api.v3.PosixTime

trait DappLedger(
    initialTreasuryUtxo: MultisigTreasuryUtxo,
    config: hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.Config,
    virtualLedger: ActorRef[IO, VirtualLedger.Request]
) extends Actor[IO, Request] {
    import DappLedger.State

    val headAddress: ShelleyAddress = initialTreasuryUtxo.address

    private val state: Ref[IO, State] =
        Ref.unsafe[IO, State](State(initialTreasuryUtxo, Queue.empty))

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: Request): IO[Unit] = req match {
        case req: SyncRequest.Any =>
            req.request match {
                case r: RegisterDeposit => r.handleSync(req, registerDeposit)
                case r: SettleLedger    => r.handleSync(req, settleLedger)
                case r: FinalizeLedger  => r.handleSync(req, finalizeLedger)
                case r: GetState.type   => r.handleSync(req, _ => state.get)
            }
    }

    /** Check that a deposit tx is valid and add the deposit utxo it produces to the ledger's state.
      * Return the produced deposit utxo and a post-dated refund transaction for it.
      */
    private def registerDeposit(
        depositSeq: RegisterDeposit
    ): EitherT[IO, RegisterDepositError, Unit] = {
        for {
            // FIXME: DepositTx's parser does not check all the invariants.
            //  Use DepositRefundTxSeq's parser, instead.
            depositTx <- EitherT
                .fromEither[IO](
                  DepositTx
                      .parse(depositSeq.serializedDeposit, config, depositSeq.virtualOutputs)
                      .left
                      .map(ParseDepositError(_))
                )
            _ <- EitherT(validateTimeBounds(depositTx))
            _ <- EitherT
                .right(
                  state
                      .update(s => s.appendToQueue((depositSeq.eventId, depositTx.depositProduced)))
                )
            _ <- EitherT.pure[IO, RegisterDepositError](())
        } yield ()
    }

    // FIXME: we are supposed be checking Deposit maturity, time bounds here. But its not clear where
    // that time actually gets set.
    private def validateTimeBounds(
        tx: DepositTx
    ): IO[Either[InvalidTimeBound, Unit]] = {
        for {
            currentTime <- realTime
            _ <-
                if tx.depositProduced.datum.refundInstructions.startTime >= BigInt(
                      currentTime.toSeconds
                    )
                then IO.pure(Left(InvalidTimeBound("deposit deadline exceeded")))
                else IO.pure(Right(()))
        } yield Right(())
    }

    /** Construct a settlement transaction, a fallback transaction, a list of rollout transactions,
      * and a list of immediate refund transactions based on the arguments. Remove the
      * absorbed/refunded deposits and update the treasury in the ledger state. Called when the
      * block weaver sends the single to close the block in leader mode.
      *
      * The collective value of the [[payouts]] must '''not''' exceed the [[treasury]] value.
      */
    // N.B.: JointLedger calls this as a synchronous request. We are not responsible for
    // sending it back to the joint ledger.
    private def settleLedger(
        args: SettleLedger
    ): EitherT[IO, SettlementTxSeqBuilderError, SettleLedger.Result] = {
        import args.*
        def isMature(depositTx: DepositUtxo): Boolean = ???

        // We use the left branch of the eitherT to short circuit if a settlement isn't actually necessary.
        // Otherwise, we return right.
        // We also use exceptions for actual exceptional circumstances.
        type ErrorOrShortCircuit = SettleLedger.ResultWithoutSettlement |
            SettlementTxSeqBuilderError

        val eResultShortCircuit
            : EitherT[IO, ErrorOrShortCircuit, SettleLedger.ResultWithSettlement] =
            for {
                s <- EitherT.right(state.get)

                // ===================================
                // Step 1: Figure out which deposits are valid and turn them into genesis obligations
                // ===================================

                // TODO: partitioning probably isn't the fastest way, because it will inspect each
                // element of the queue. But I don't recall if we assume the queue is sorted according to
                // maturity time, so I'll go with this for now. If it is sorted, there's almost certainly
                // a more efficient function.
                depositsPartition = s.deposits.partition(x => isMature(x._2))
                matureDeposits = depositsPartition._1
                immatureDeposits = depositsPartition._2

                // Tuple containing (depositsInPollResults, depositsNotInPollResults)
                depositPartition = matureDeposits.partition(x => pollDepositResults.contains(x._1))
                depositsInPollResults = depositPartition._1

                // TODO: these just get ignored for now. In the future, we'd want to create a RefundImmediate
                depositsNotInPollResults = depositPartition._2

                _ <-
                    if depositsInPollResults.isEmpty && payoutObligations.isEmpty
                    then
                        EitherT.leftT[IO, SettleLedger.ResultWithSettlement](
                          SettleLedger.ResultWithoutSettlement(depositsNotInPollResults)
                        )
                    else EitherT.pure[IO, SettleLedger.ResultWithoutSettlement](())

                genesisObligations: Queue[(LedgerEvent.Id, NonEmptyList[GenesisObligation])] =
                    if depositsInPollResults.isEmpty then Queue.empty
                    else
                        depositsInPollResults.map(d =>
                            (
                              d._1,
                              d._2.virtualOutputs
                            )
                        )

                // ===================================
                // Step 2: determine the necessary KZG commit
                // ===================================
                kzgCommit <- EitherT.right(virtualLedger ?: VirtualLedger.GetCurrentKzgCommitment)

                // ===================================
                // Step 3: Build up a settlement Tx
                // ===================================

                settlementTxSeqArgs = SettlementTxSeq.Builder.Args(
                  kzgCommitment = kzgCommit,
                  majorVersionProduced =
                      Block.Version.Major(s.treasury.datum.versionMajor.toInt).increment,
                  treasuryToSpend = s.treasury,
                  depositsToSpend = depositsInPollResults.map(_._2).toVector,
                  payoutObligationsRemaining = payoutObligations,
                  tallyFeeAllowance = tallyFeeAllowance,
                  votingDuration = votingDuration,
                  competingFallbackValidityStart = ???,
                  blockCreatedOn = ???,
                  txTiming = ???
                )

                settlementTxSeqRes <-
                    EitherT
                        .fromEither[IO](
                          SettlementTxSeq
                              .Builder(config)
                              .build(settlementTxSeqArgs)
                              .left
                              .map(SettlementTxSeqBuilderError.apply)
                        )

                // ===================================
                // Step 4: build the result
                // ===================================

                // We update the state with:
                // - the treasury produced by the settlement tx
                // - The deposits that were _not_ successfully processed by the settlement transaction (due to not fitting)
                //   and the remaining immature deposits
                newState = State(
                  treasury = settlementTxSeqRes.settlementTxSeq.settlementTx.treasuryProduced,
                  deposits = {
                      // The remaining "depositsToSpend" reattached to their associated refunds.
                      // (The settlement tx builder loses this information)
                      val correlatedDeposits =
                          matureDeposits.filter(x =>
                              settlementTxSeqRes.depositsToSpend.contains(x._1)
                          )
                      correlatedDeposits ++ immatureDeposits
                  }
                )
                _ <- EitherT.right(state.set(newState))
                _ <- EitherT.pure[IO, ErrorOrShortCircuit](())
            } yield SettleLedger.ResultWithSettlement(
              settlementTxSeqRes.settlementTxSeq,
              settlementTxSeqRes.fallbackTx,
              genesisObligations,
              depositsNotInPollResults
            )

        eResultShortCircuit.leftFlatMap {
            case txSeq: SettleLedger.ResultWithoutSettlement => EitherT.rightT(txSeq)
            case e: SettlementTxSeqBuilderError              => EitherT.leftT(e)
        }
    }

    /** Construct a finalization transaction, a list of rollout transactions, and a list of
      * immediate refund transactions based on the arguments. The [[DappLedger]] must be discarded
      * after this, so there's no point in updating its state.
      *
      * The collective value of the [[payouts]] must '''not''' exceed the [[treasury]] value.
      * Immediate refund transactions must be constructed for every deposit in the ledger state.
      */
    // TODO (fund14): add Refund.Immediates to the return type
    private def finalizeLedger(
        args: FinalizeLedger
    ): EitherT[IO, FinalizationTxSeqBuilderError, FinalizationTxSeq] = {
        import args.*
        for {
            s <- EitherT.right(state.get)
            kzg: KzgCommitment <- EitherT(???)
            args = FinalizationTxSeq.Builder.Args(
              kzgCommitment = kzg,
              majorVersionProduced =
                  Block.Version.Major(s.treasury.datum.versionMajor.toInt).increment,
              treasuryToSpend = s.treasury,
              payoutObligationsRemaining = payoutObligationsRemaining,
              multisigRegimeUtxoToSpend = multisigRegimeUtxoToSpend,
              equityShares = equityShares,
                  competingFallbackValidityStart = ???,
                  blockCreatedOn = ???,
                  txTiming = ???
                )
                ftxSeq <- EitherT.fromEither[IO](FinalizationTxSeq.Builder(config).build(args)
                  .left
                  .map(FinalizationTxSeqBuilderError.apply)
            )
        } yield ftxSeq
    }
}

/** ==Hydrozoa's detached dapp ledger on Cardano in the multisig regime==
  *
  * '''Dapp ledger on Cardano''' means that the ledger is domain-specific to a single decentralized
  * application (dApp), and that its state corresponds to a subset of the utxos in the general
  * Cardano ledger. Every state transition of the dapp ledger corresponds to a sequence of one or
  * more Cardano transactions.
  *
  * '''Detached dapp ledger''' means that the ledger's state can be evolved without waiting to
  * synchronize each state transition with Cardano. Instead, the Cardano transactions can be
  * asynchronously submitted to drive Cardano toward the state corresponding to the dapp ledger,
  * repeatedly re-submitting transactions as necessary until they are confirmed on Cardano.
  *
  * Hydrozoa's consensus protocol makes it possible for its dapp ledger to be detached by mitigating
  * the sources of contention that might interfere with the Cardano transactions corresponding to
  * the dapp ledger's transitions.
  */
object DappLedger {
    final case class State(
        treasury: MultisigTreasuryUtxo,
        // TODO: Queue[(EventId, DepositUtxo, RefundTx.PostDated)]
        deposits: Queue[(LedgerEvent.Id, DepositUtxo)] = Queue()
    ) {
        def appendToQueue(t: (LedgerEvent.Id, DepositUtxo)): State =
            this.copy(treasury, deposits.appended(t))
    }

    object Requests {
        type Request = RegisterDeposit.Sync | SettleLedger.Sync | FinalizeLedger.Sync |
            GetState.Sync

        // FIXME: This should include the refundTxBytes
        // FIXME: The virtual outputs should not be parsed yet (i.e. Array[Byte])
        final case class RegisterDeposit(
            serializedDeposit: Array[Byte],
            eventId: LedgerEvent.Id,
            virtualOutputs: NonEmptyList[GenesisObligation],
        ) extends SyncRequestE[IO, RegisterDeposit, RegisterDepositError, Unit] {
            export RegisterDeposit.Sync
            def ?: : this.Send = SyncRequest.send(_, this)
        }

        object RegisterDeposit {
            type Sync = SyncRequest.EnvelopeE[IO, RegisterDeposit, RegisterDepositError, Unit]
        }

        final case class SettleLedger(
            pollDepositResults: Set[LedgerEvent.Id],
            payoutObligations: Vector[Payout.Obligation],
            blockCreationTime: PosixTime,
            tallyFeeAllowance: Coin,
            votingDuration: PosixTime
        ) extends SyncRequestE[
              IO,
              SettleLedger,
              SettlementTxSeqBuilderError,
              SettleLedger.Result
            ] {
            export SettleLedger.Sync
            def ?: : this.Send = SyncRequest.send(_, this)
        }

        object SettleLedger {
            type Sync = SyncRequest.EnvelopeE[
              IO,
              SettleLedger,
              SettlementTxSeqBuilderError,
              SettleLedger.Result
            ]

            /** Sum type for results of calls to SettleLedger
              */
            trait Result

            /** Returned if either a deposit absorption or withdrawal is necessary to settle the
              * ledger
              */
            final case class ResultWithSettlement(
                settlementTxSeq: SettlementTxSeq,
                fallBack: FallbackTx,
                absorbedDeposits: Queue[(LedgerEvent.Id, NonEmptyList[GenesisObligation])],
                refundedDeposits: Queue[(LedgerEvent.Id, DepositUtxo)]
            ) extends Result

            /** Returned if no deposit absorptions or withdrawals are necessary to settle the ledger
              */
            final case class ResultWithoutSettlement(
                refundedDeposits: Queue[(LedgerEvent.Id, DepositUtxo)]
            ) extends Result
        }

        final case class FinalizeLedger(
            payoutObligationsRemaining: Vector[Payout.Obligation],
            multisigRegimeUtxoToSpend: MultisigRegimeUtxo,
            equityShares: EquityShares
        ) extends SyncRequestE[
              IO,
              FinalizeLedger,
              FinalizationTxSeqBuilderError,
              FinalizationTxSeq
            ] {
            export FinalizeLedger.Sync
            def ?: : this.Send = SyncRequest.send(_, this)
        }

        object FinalizeLedger {
            type Sync = SyncRequest.EnvelopeE[
              IO,
              FinalizeLedger,
              FinalizationTxSeqBuilderError,
              FinalizationTxSeq
            ]
        }

        case object GetState extends SyncRequest[IO, GetState.type, State] {
            type Sync = SyncRequest.Envelope[IO, GetState.type, State]
            def ?: : this.Send = SyncRequest.send(_, this)
        }

    }

    /** Initialize the L1 ledger's state and return the corresponding initialization transaction. */
    def create(
        initTx: InitializationTx,
        config: hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.Config,
        virtualLedger: ActorRef[IO, VirtualLedger.Request]
    ): DappLedger =
        new DappLedger(initialTreasuryUtxo = initTx.treasuryProduced, config, virtualLedger) {}

    object Errors {
        sealed trait DappLedgerError

        sealed trait RegisterDepositError extends DappLedgerError

        final case class ParseDepositError(wrapped: DepositTx.ParseError)
            extends RegisterDepositError

        final case class InvalidTimeBound(msg: String) extends RegisterDepositError

        final case class ParseRefundPostDatedError(wrapped: String) extends RegisterDepositError

        final case class SettlementTxSeqBuilderError(wrapped: SettlementTxSeq.Builder.Error)
            extends DappLedgerError

        final case class FinalizationTxSeqBuilderError(wrapped: FinalizationTxSeq.Builder.Error)
            extends DappLedgerError
    }
}
