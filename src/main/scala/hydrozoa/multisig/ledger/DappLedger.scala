package hydrozoa.multisig.ledger

import cats.data.EitherT
import cats.effect.IO.realTime
import cats.effect.{Deferred, IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ReplyingActor
import hydrozoa.config.EquityShares
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.multisig.ledger.DappLedger.*
import hydrozoa.multisig.ledger.DappLedger.Errors.*
import hydrozoa.multisig.ledger.DappLedger.Requests.*
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.txseq.{FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigRegimeUtxo, TreasuryUtxo}
import hydrozoa.multisig.ledger.joint.utxo.Payout
import hydrozoa.multisig.ledger.virtual.GenesisObligation
import hydrozoa.multisig.protocol.types.{Block, LedgerEvent}
import scala.collection.immutable.Queue
import scala.language.implicitConversions
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.ledger.api.v3.PosixTime

trait DappLedger(
    initialTreasuryUtxo: TreasuryUtxo,
    config: hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.Config
) extends Actor[IO, Request] {
    val headAddress: ShelleyAddress = initialTreasuryUtxo.address

    private val state: Ref[IO, State] =
        Ref.unsafe[IO, State](State(initialTreasuryUtxo, Queue.empty))

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction {
        case d: RegisterDeposit => d.handleRequest(registerDeposit)
        case s: SettleLedger    => s.handleRequest(settleLedger)
        case f: FinalizeLedger  => finalizeLedger(f)
        // FIXME: this can't actually throw an error, not really. But using ?: makes it seem like it can
        case g: GetState => g.handleRequest(_ => state.get)
    }

    /** Check that a deposit tx is valid and add the deposit utxo it produces to the ledger's state.
      * Return the produced deposit utxo and a post-dated refund transaction for it.
      */
    private def registerDeposit(depositSeq: RegisterDeposit): IO[Unit] = {
        val eitherTxs: Either[ParseDepositError, DepositTx] = for {
            depositTx <- DepositTx
                .parse(depositSeq.serializedDeposit)
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
                        .update(s => s.appendToQueue((depositSeq._2, x.depositProduced)))
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
                if tx.depositProduced.datum.deadline < BigInt(currentTime.toSeconds)
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
                    if depositsInPollResults.isEmpty && payouts.isEmpty
                    then
                        EitherT.leftT[IO, SettleLedger.ResultWithSettlement](
                          SettleLedger.ResultWithoutSettlement(depositsNotInPollResults)
                        )
                    else EitherT.pure[IO, SettleLedger.ResultWithoutSettlement](())

                genesisObligations: Queue[(LedgerEvent.Id, GenesisObligation)] =
                    if depositsInPollResults.isEmpty then Queue.empty
                    else
                        depositsInPollResults.map(d =>
                            (
                              d._1,
                              GenesisObligation
                                  .fromDepositUtxo(d._2, network = config.headAddress.network)
                            )
                        )

                /* FIXME:
            At this point, I'm fairly certain that the DappLedger needs to call out to the virtual ledger
            in order to pass it the genesis obligations. We need all of the obligations in this Tx applied
            in order to calculate the KZG commit, which we subsequently need to get in a synchronous call via
                virtualLedger ?: GetState
            and the settlement tx builder needs to be updated to take the KZG commit
                 */
                settlementTxSeqArgs = SettlementTxSeq.Builder.Args(
                  // Is this field redundant?
                  majorVersionProduced =
                      Block.Version.Major(s.treasury.datum.versionMajor.toInt).increment,
                  treasuryToSpend = s.treasury,
                  depositsToSpend = depositsInPollResults.map(_._2).toVector,
                  payoutObligationsRemaining = payouts,
                  tallyFeeAllowance = tallyFeeAllowance,
                  votingDuration = votingDuration
                )

                settlementTxSeqRes <-
                    SettlementTxSeq.Builder(config).build(settlementTxSeqArgs) match {
                        case Left(e)  => throw SettlementTxSeqBuilderError(e)
                        case Right(r) => EitherT.right(IO.pure(r))
                    }

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
    def finalizeLedger(
        args: FinalizeLedger
    ): IO[Either[FinalizationTxSeq.Builder.Error, FinalizationTxSeq]] = {
        import args.*
        val eitherT: EitherT[IO, FinalizationTxSeq.Builder.Error, FinalizationTxSeq] =
            for {
                s <- EitherT.right(state.get)
                args = FinalizationTxSeq.Builder.Args(
                  majorVersionProduced =
                      Block.Version.Major(s.treasury.datum.versionMajor.toInt).increment,
                  treasuryToSpend = s.treasury,
                  payoutObligationsRemaining = payoutObligationsRemaining,
                  multisigRegimeUtxoToSpend = multisigRegimeUtxoToSpend,
                  equityShares = equityShares
                )
                ftxSeq <- EitherT.fromEither[IO](FinalizationTxSeq.Builder(config).build(args))
            } yield ftxSeq
        eitherT.value
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
            override val dResponse: Deferred[IO, Either[ParseDepositError, Unit]]
        ) extends SyncRequest[IO, ParseDepositError, Unit]

        final case class SettleLedger private (
            pollDepositResults: Set[LedgerEvent.Id],
            payouts: Vector[Payout.Obligation.L1],
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
                eventId: LedgerEvent.Id
            ): IO[RegisterDeposit] =
                Deferred[IO, Either[ParseDepositError, Unit]]
                    .flatMap(x => IO.pure(new RegisterDeposit(serializedDeposit, eventId, x)))
        }

        object SettleLedger {
            def apply(
                pollDepositResults: Set[LedgerEvent.Id],
                payouts: Vector[Payout.Obligation.L1],
                blockCreationTime: PosixTime,
                tallyFeeAllowance: Coin,
                votingDuration: PosixTime
            ): IO[SettleLedger] =
                Deferred[IO, Either[SettlementTxSeqBuilderError, SettleLedger.Result]]
                    .flatMap(x =>
                        IO.pure(
                          new SettleLedger(
                            pollDepositResults = pollDepositResults,
                            payouts = payouts,
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
              *
              * @param settlementTxSeq
              * @param absorbedDeposits
              * @param refundedDeposits
              */
            final case class ResultWithSettlement(
                settlementTxSeq: SettlementTxSeq,
                fallBack: FallbackTx,
                absorbedDeposits: Queue[(LedgerEvent.Id, GenesisObligation)],
                refundedDeposits: Queue[(LedgerEvent.Id, DepositUtxo)]
            ) extends Result

            /** Returned if no deposit absorptions or withdrawals are necessary to settle the ledger
              *
              * @param refundedDeposits
              */
            final case class ResultWithoutSettlement(
                refundedDeposits: Queue[(LedgerEvent.Id, DepositUtxo)]
            ) extends Result
        }

        final case class FinalizeLedger(
            payoutObligationsRemaining: Vector[Payout.Obligation.L1],
            multisigRegimeUtxoToSpend: MultisigRegimeUtxo,
            equityShares: EquityShares,
        )

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
    // TODO: We actually want to pass a pre-formed Initialization Tx into the dapp ledger to create it.
    // Or perhaps just the treasury UTxO? Either way, we still need a builder config.
    def create(
        initTx: InitializationTx,
        config: hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.Config
    ): DappLedger =
        new DappLedger(initialTreasuryUtxo = initTx.treasuryProduced, config) {}

    trait Tx {
        def tx: Transaction

        /** A transaction belongs to a [[DappLedger]] if it matches on address and currency symbol
          */
        def txBelongsToLedger(ledger: DappLedger): Boolean =
            this.tx.auxiliaryData.getOrElse(false) match {
                case Metadata(m) =>
                    m.get(TransactionMetadatumLabel(CIP67.Tags.head))
                        .fold(false)(_ == ledger.headAddress)
                case _ => false
            }
    }

    object Errors {
        sealed trait DappLedgerError extends Throwable

        final case class ParseDepositError(wrapped: DepositTx.ParseError) extends DappLedgerError

        final case class ParseRefundPostDatedError(wrapped: String) extends DappLedgerError

        final case class InvalidTimeBound(msg: String) extends DappLedgerError

        final case class SettlementTxSeqBuilderError(wrapped: SettlementTxSeq.Builder.Error)
            extends DappLedgerError

        case object GetStateError extends DappLedgerError
    }

    object Tx {
        type Serialized = Array[Byte]
    }
}
