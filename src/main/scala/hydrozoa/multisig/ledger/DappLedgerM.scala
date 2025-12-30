package hydrozoa.multisig.ledger

import cats.data.*
import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.config.EquityShares
import hydrozoa.multisig.ledger
import hydrozoa.multisig.ledger.DappLedgerM.Error.{ParseDepositError, SettlementTxSeqBuilderError}
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.txseq.{FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.GenesisObligation
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.protocol.types.{Block, LedgerEvent}
import monocle.syntax.all.*
import scala.collection.immutable.Queue
import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions
import scalus.cardano.ledger.*

private type E[A] = Either[DappLedgerM.Error, A]
private type S[A] = cats.data.StateT[E, DappLedgerM.State, A]
private type RT[A] = ReaderT[S, Tx.Builder.Config, A]

/** DappLedgerM defines an opaque monad stack for manipulating the [[State]] of the dApp ledger.
  * It's constructor and eliminator methods are private so that it cannot be used in unintended
  * ways.
  *
  * See the companion object for details on allowed operations.
  */
case class DappLedgerM[A] private (private val unDappLedger: RT[A]) {

    import DappLedgerM.*

    private def map[B](f: A => B): DappLedgerM[B] = DappLedgerM(this.unDappLedger.map(f))

    private def flatMap[B](f: A => DappLedgerM[B]): DappLedgerM[B] =
        DappLedgerM(this.unDappLedger.flatMap(a => f(a).unDappLedger))

    /** Use [[runDappLedgerM()]] instead
      * @param config
      * @param initialState
      * @return
      */
    private def run(
        config: Tx.Builder.Config,
        initialState: DappLedgerM.State
    ): Either[DappLedgerM.Error, (DappLedgerM.State, A)] =
        this.unDappLedger.run(config).run(initialState)

}

object DappLedgerM {

    /** Extract the transaction builder configuration from a [[DappLedgerM]]
      */
    private val ask: DappLedgerM[Tx.Builder.Config] =
        DappLedgerM(Kleisli.ask)

    /** Obtain the current state from a [[DappLedgerM]]
      */
    private val get: DappLedgerM[DappLedgerM.State] =
        DappLedgerM(Kleisli.liftF(cats.data.StateT.get))

    /** Set the state of the [[DappLedgerM]] state machine
      */
    private def set(newState: ledger.DappLedgerM.State): DappLedgerM[Unit] =
        DappLedgerM(Kleisli.liftF(cats.data.StateT.set(newState)))

    /** Lift an [[Either]] into a [[DappLedgerM]].
      */
    private def lift[A](e: Either[DappLedgerM.Error, A]): DappLedgerM[A] =
        DappLedgerM(Kleisli.liftF(StateT.liftF(e)))

    /** Check that a deposit tx is valid and add the deposit utxo it produces to the ledger's state.
      */
    // TODO: Return the produced deposit utxo and a post-dated refund transaction for it.
    def registerDeposit(
        serializedDeposit: Array[Byte],
        eventId: LedgerEvent.Id,
        virtualOutputs: NonEmptyList[GenesisObligation]
    ): DappLedgerM[Unit] = {
        for {
            config <- ask
            // FIXME: DepositTx's parser does not check all the invariants.
            //  Use DepositRefundTxSeq's parser, instead.
            depositTx <- lift(
              DepositTx
                  .parse(serializedDeposit, config, virtualOutputs)
                  .left
                  .map(ParseDepositError(_))
            )
            // _ <- EitherT(validateTimeBounds(depositTx))
            s <- get
            newState = s.appendToQueue((eventId, depositTx.depositProduced))
            _ <- set(newState)
        } yield ()
    }

    /** Construct a settlement transaction, a fallback transaction, a list of rollout transactions,
      * and a list of immediate refund transactions based on the arguments. Remove the
      * absorbed/refunded deposits and update the treasury in the ledger state. Called when the
      * block weaver sends the single to close the block in leader mode.
      *
      * The collective value of the [[payouts]] must '''not''' exceed the [[treasury]] value.
      */
    def settleLedger(
        nextKzg: KzgCommitment,
        validDeposits: Queue[(LedgerEvent.Id, DepositUtxo)],
        payoutObligations: Vector[Payout.Obligation],
        tallyFeeAllowance: Coin,
        votingDuration: FiniteDuration,
        immatureDeposits: Queue[(LedgerEvent.Id, DepositUtxo)],
        blockCreatedOn: FiniteDuration,
        competingFallbackValidityStart: FiniteDuration,
        txTiming: TxTiming
    ): DappLedgerM[SettleLedger.Result] = {

        for {
            config <- ask
            state <- get

            args = SettlementTxSeq.Builder.Args(
              kzgCommitment = nextKzg,
              majorVersionProduced =
                  Block.Version.Major(state.treasury.datum.versionMajor.toInt + 1),
              treasuryToSpend = state.treasury,
              depositsToSpend = Vector.from(validDeposits.map(_._2).toList),
              payoutObligationsRemaining = payoutObligations,
              tallyFeeAllowance = tallyFeeAllowance,
              votingDuration = votingDuration,
              competingFallbackValidityStart = competingFallbackValidityStart,
              blockCreatedOn = blockCreatedOn,
              txTiming = txTiming
            )

            settlementTxSeqRes <- lift(
              SettlementTxSeq
                  .Builder(config)
                  .build(args)
                  .left
                  .map(SettlementTxSeqBuilderError.apply)
            )

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
                      Queue.from(
                        validDeposits.filter(x => settlementTxSeqRes.depositsToSpend.contains(x._1))
                      )
                  correlatedDeposits ++ immatureDeposits
              }
            )
            _ <- set(newState)
        } yield SettleLedger.Result(
          settlementTxSeqRes.settlementTxSeq,
          settlementTxSeqRes.fallbackTx,
        )

    }

    /** Construct a finalization transaction, a list of rollout transactions, and a list of
      * immediate refund transactions based on the arguments. The [[DappLedgerM]] must be discarded
      * after this, so there's no point in updating its state.
      *
      * The collective value of the [[payouts]] must '''not''' exceed the [[treasury]] value.
      * Immediate refund transactions must be constructed for every deposit in the ledger state.
      */
    // TODO (fund14): add Refund.Immediates to the return type
    def finalizeLedger(
        payoutObligationsRemaining: Vector[Payout.Obligation],
        multisigRegimeUtxoToSpend: MultisigRegimeUtxo,
        equityShares: EquityShares,
//        blockCreatedOn : PosixTime,
//        competingFallbackValidityStart : PosixTime,
//        txTiming: TxTiming
    ): DappLedgerM[FinalizationTxSeq] = {
        for {
            s <- get
            config <- ask
//            kzg: KzgCommitment <- DappLedger.(???)
            args = FinalizationTxSeq.Builder.Args(
              kzgCommitment = ???,
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
            ftxSeq <- lift(
              FinalizationTxSeq
                  .Builder(config)
                  .build(args)
                  .left
                  .map(Error.FinalizationTxSeqBuilderError.apply)
            )
        } yield ftxSeq
    }

    final case class State(
        treasury: MultisigTreasuryUtxo,
        // TODO: Queue[(EventId, DepositUtxo, RefundTx.PostDated)]
        deposits: Queue[(LedgerEvent.Id, DepositUtxo)] = Queue()
    ) {
        def appendToQueue(t: (LedgerEvent.Id, DepositUtxo)): State =
            this.copy(treasury, deposits.appended(t))
    }

    object SettleLedger {
        final case class Result(
            settlementTxSeq: SettlementTxSeq,
            fallBack: FallbackTx,
        )
    }

    sealed trait Error
    object Error {

        sealed trait RegisterDepositError extends DappLedgerM.Error

        final case class ParseDepositError(wrapped: DepositTx.ParseError)
            extends RegisterDepositError

        final case class InvalidTimeBound(msg: String) extends RegisterDepositError

        final case class ParseRefundPostDatedError(wrapped: String) extends RegisterDepositError

        final case class SettlementTxSeqBuilderError(wrapped: SettlementTxSeq.Builder.Error)
            extends DappLedgerM.Error

        final case class FinalizationTxSeqBuilderError(wrapped: FinalizationTxSeq.Builder.Error)
            extends DappLedgerM.Error
    }

    extension (jl: JointLedger) {

        /** Run a DappLedgerM action within a JointLedger. If the action is successful (returns
          * `Right`), the state of the JointLedger is (unconditionally) updated. Because the state
          * update within JointLedger must happen within [[IO]], this takes two continuations (one
          * for success, one for failure) and returns in [[IO]].
          *
          * @param action
          * @param onFailure
          *   continuation if an error is raised. Defaults to throwing an exception.
          * @param onSuccess
          *   continuation if a value is returned.
          * @tparam A
          * @return
          */
        def runDappLedgerM[A, B](
            action: DappLedgerM[A],
            onFailure: DappLedgerM.Error => IO[B] = e =>
                // FIXME: type the exception better
                throw new RuntimeException(s"Error running DappLedgerM: $e"),
            onSuccess: A => IO[B]
        ): IO[B] =
            for {
                oldState <- jl.state.get
                res = action.run(jl.config, oldState.dappLedgerState)
                b <- res match {
                    case Left(error) => onFailure(error)
                    case Right(newState, a) =>
                        for {
                            _ <- jl.state.set(oldState match {
                                case d: JointLedger.Done =>
                                    d.focus(_.dappLedgerState).replace(newState)
                                case p: JointLedger.Producing =>
                                    p.focus(_.dappLedgerState).replace(newState)
                            })
                            b <- onSuccess(a)
                        } yield b
                }
            } yield b
    }
}
