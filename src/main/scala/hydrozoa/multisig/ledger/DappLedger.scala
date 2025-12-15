package hydrozoa.multisig.ledger

import cats.data.{EitherT, NonEmptyList}
import cats.effect.IO.realTime
import cats.effect.{Deferred, IO, Ref}
import cats.implicits.catsSyntaxFlatMapOps
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.actor.ReplyingActor
import hydrozoa.config.EquityShares
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.multisig.ledger.DappLedger.Errors.*
import hydrozoa.multisig.ledger.DappLedger.Requests.*
import hydrozoa.multisig.ledger.DappLedger.{State, *}
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.txseq.{FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigRegimeUtxo, TreasuryUtxo}
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
    initialTreasuryUtxo: TreasuryUtxo,
    config: hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.Config,
    virtualLedger: ActorRef[IO, VirtualLedger.Request]
) extends Actor[IO, Request] {
    val headAddress: ShelleyAddress = initialTreasuryUtxo.address

    private val state: Ref[IO, State] =
        Ref.unsafe[IO, State](State(initialTreasuryUtxo, Queue.empty))

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction {
        case d: RegisterDeposit => d.handleRequest(registerDeposit)
        case s: SettleLedger    => s.handleRequest(settleLedger)
        case f: FinalizeLedger  => f.handleRequest(finalizeLedger)
        // FIXME: this can't actually throw an error, not really. But using ?: makes it seem like it can
        case g: GetState => g.handleRequest(_ => state.get)
    }

    /** Check that a deposit tx is valid and add the deposit utxo it produces to the ledger's state.
      * Return the produced deposit utxo and a post-dated refund transaction for it.
      */
    private def registerDeposit(depositSeq: RegisterDeposit): IO[Unit] = {
        val eitherTxs: Either[ParseDepositError, DepositTx] = for {
            depositTx <- DepositTx
                .parse(depositSeq.serializedDeposit, config, depositSeq.virtualOutputs)
                .left
                .map(ParseDepositError(_))
            //            refundTx: RefundTx.PostDated <- RefundTx.PostDated
            //                .parse(depositSeq.serializedRefund)
            //                .left
            //                .map(ParseRefundPostDatedError(_))
        } yield depositTx // , refundTx)

        eitherTxs match {
            case Left(e) => throw e
            case Right(x) =>
                for {
                    _ <- validateTimeBounds(x)
                    _ <- state
                        .update(s => s.appendToQueue((depositSeq.eventId, x.depositProduced)))
                        .map(Right(_))
                } yield ()
        }
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
    ): IO[SettleLedger.Result] = {

        import args.*
        def isMature(depositTx: DepositUtxo): Boolean = ???

        // We use the left branch of the eitherT to short circuit if a settlement isn't actually necessary.
        // Otherwise, we return right.
        // We also use exceptions for actual exceptional circumstances.
        val eitherT
            : EitherT[IO, SettleLedger.ResultWithoutSettlement, SettleLedger.ResultWithSettlement] =
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

                gsReq <- EitherT.right(VirtualLedger.GetCurrentKzgCommitment())
                kzgCommit <- EitherT.right((virtualLedger ?: gsReq) >>= {
                    case Left(e)  => throw GetKzgError
                    case Right(r) => IO.pure(r)
                })

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
                  votingDuration = votingDuration
                )

                settlementTxSeqRes <-
                    SettlementTxSeq.Builder(config).build(settlementTxSeqArgs) match {
                        case Left(e)  => throw SettlementTxSeqBuilderError(e)
                        case Right(r) => EitherT.right(IO.pure(r))
                    }

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
            } yield SettleLedger.ResultWithSettlement(
              settlementTxSeqRes.settlementTxSeq,
              settlementTxSeqRes.fallbackTx,
              genesisObligations,
              depositsNotInPollResults
            )
        eitherT.value.map {
            case Left(l)  => l
            case Right(r) => r
        }
    }

    /** Construct a finalization transaction, a list of rollout transactions, and a list of
      * immediate refund transactions based on the arguments. The [[DappLedger]] must be discarded
      * after this, so there's no point in updating its state.
      *
      * @param payouts
      *   a list of payout outputs that should be produced by the finalization and rollout
      *   transactions.
      *
      * The collective value of the [[payouts]] must '''not''' exceed the [[treasury]] value.
      * Immediate refund transactions must be constructed for every deposit in the ledger state.
      */
    // TODO (fund14): add Refund.Immediates to the return type
    private def finalizeLedger(
        args: FinalizeLedger
    ): IO[FinalizationTxSeq] = {
        import args.*
        val eitherT: EitherT[IO, FinalizationTxSeq.Builder.Error, FinalizationTxSeq] =
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
                  equityShares = equityShares
                )
                ftxSeq <- EitherT.fromEither[IO](FinalizationTxSeq.Builder(config).build(args))
            } yield ftxSeq
        eitherT.value.map {
            case Left(e)  => throw FinalizationTxSeqBuilderError(e)
            case Right(r) => r
        }
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
        treasury: TreasuryUtxo,
        // TODO: Queue[(EventId, DepositUtxo, RefundTx.PostDated)]
        deposits: Queue[(LedgerEvent.Id, DepositUtxo)] = Queue()
    ) {
        def appendToQueue(t: (LedgerEvent.Id, DepositUtxo)): State =
            this.copy(treasury, deposits.appended(t))
    }

    object Requests {
        type Request = RegisterDeposit | SettleLedger | FinalizeLedger | GetState

        final case class RegisterDeposit private (
            serializedDeposit: Array[Byte],
            eventId: LedgerEvent.Id,
            virtualOutputs: NonEmptyList[GenesisObligation],
            override val dResponse: Deferred[IO, Either[ParseDepositError, Unit]]
        ) extends SyncRequest[IO, ParseDepositError, Unit]

        final case class SettleLedger private (
            pollDepositResults: Set[LedgerEvent.Id],
            payoutObligations: Vector[Payout.Obligation],
            blockCreationTime: PosixTime,
            tallyFeeAllowance: Coin,
            votingDuration: PosixTime,
            override val dResponse: Deferred[
              IO,
              Either[SettlementTxSeqBuilderError, SettleLedger.Result]
            ]
        ) extends SyncRequest[IO, SettlementTxSeqBuilderError, SettleLedger.Result]

        object RegisterDeposit {
            // TODO: Make opaque? We don't want just any random deposit with any random event ID, we
            // want to make sure that they correspond to each other
            def apply(
                serializedDeposit: Array[Byte],
                eventId: LedgerEvent.Id,
                virtualOutputs: NonEmptyList[GenesisObligation]
            ): IO[RegisterDeposit] =
                Deferred[IO, Either[ParseDepositError, Unit]]
                    .flatMap(x =>
                        IO.pure(new RegisterDeposit(serializedDeposit, eventId, virtualOutputs, x))
                    )
        }

        object SettleLedger {
            def apply(
                pollDepositResults: Set[LedgerEvent.Id],
                payoutObligations: Vector[Payout.Obligation],
                blockCreationTime: PosixTime,
                tallyFeeAllowance: Coin,
                votingDuration: PosixTime
            ): IO[SettleLedger] =
                Deferred[IO, Either[SettlementTxSeqBuilderError, SettleLedger.Result]]
                    .flatMap(x =>
                        IO.pure(
                          new SettleLedger(
                            pollDepositResults = pollDepositResults,
                            payoutObligations = payoutObligations,
                            blockCreationTime = blockCreationTime,
                            tallyFeeAllowance = tallyFeeAllowance,
                            votingDuration = votingDuration,
                            dResponse = x
                          )
                        )
                    )

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

        final case class FinalizeLedger private (
            payoutObligationsRemaining: Vector[Payout.Obligation],
            multisigRegimeUtxoToSpend: MultisigRegimeUtxo,
            equityShares: EquityShares,
            override val dResponse: Deferred[
              IO,
              Either[FinalizationTxSeqBuilderError, FinalizationTxSeq]
            ]
        ) extends SyncRequest[IO, FinalizationTxSeqBuilderError, FinalizationTxSeq]

        object FinalizeLedger {
            def apply(
                payoutObligationsRemaining: Vector[Payout.Obligation],
                multisigRegimeUtxoToSpend: MultisigRegimeUtxo,
                equityShares: EquityShares
            ): IO[FinalizeLedger] = for {
                deferred <- Deferred[IO, Either[FinalizationTxSeqBuilderError, FinalizationTxSeq]]
            } yield FinalizeLedger(
              payoutObligationsRemaining,
              multisigRegimeUtxoToSpend = multisigRegimeUtxoToSpend,
              equityShares = equityShares,
              dResponse = deferred
            )
        }

        final case class GetState(
            override val dResponse: Deferred[IO, Either[Errors.GetStateError.type, State]]
        ) extends SyncRequest[IO, Errors.GetStateError.type, State]

        object GetState {
            def apply(): IO[GetState] = for {
                deferred <- Deferred[IO, Either[Errors.GetStateError.type, State]]
            } yield GetState(deferred)
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
        sealed trait DappLedgerError extends Throwable

        final case class ParseDepositError(wrapped: DepositTx.ParseError) extends DappLedgerError

        final case class ParseRefundPostDatedError(wrapped: String) extends DappLedgerError

        final case class InvalidTimeBound(msg: String) extends DappLedgerError

        final case class SettlementTxSeqBuilderError(wrapped: SettlementTxSeq.Builder.Error)
            extends DappLedgerError

        final case class FinalizationTxSeqBuilderError(wrapped: FinalizationTxSeq.Builder.Error)
            extends DappLedgerError

        case object GetStateError extends DappLedgerError

        // NOTE: at the time of writing, this shouldn't be able to throw an error (except in the general
        // sense that any IO request can throw an error). But the API currently requires us to account for
        // errors, so we do
        case object GetKzgError extends DappLedgerError
    }
}
