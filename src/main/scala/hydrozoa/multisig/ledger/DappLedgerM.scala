package hydrozoa.multisig.ledger

import cats.data.*
import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.config.EquityShares
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.multisig.ledger
import hydrozoa.multisig.ledger.DappLedgerM.Error.{AbsorptionPeriodExpired, ParseError, SettlementTxSeqBuilderError}
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.txseq
import hydrozoa.multisig.ledger.dapp.txseq.{DepositRefundTxSeq, FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.event.LedgerEvent.RegisterDeposit
import hydrozoa.multisig.ledger.event.LedgerEventId
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import monocle.syntax.all.*
import scala.collection.immutable.Queue
import scala.language.implicitConversions
import scala.math.Ordered.orderingToOrdered
import scalus.cardano.ledger.*

private type E[A] = Either[DappLedgerM.Error, A]
private type S[A] = cats.data.StateT[E, DappLedgerM.State, A]
private type RT[A] = ReaderT[S, DappLedgerM.Config, A]

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
        config: DappLedgerM.Config,
        initialState: DappLedgerM.State
    ): Either[DappLedgerM.Error, (DappLedgerM.State, A)] =
        this.unDappLedger.run(config).run(initialState)

}

object DappLedgerM {
    case class Config(
        txTiming: TxTiming,
        equityShares: EquityShares,
        multisigRegimeUtxo: MultisigRegimeUtxo,
        headMultisigScript: HeadMultisigScript,
        cardanoInfo: CardanoInfo,
        tokenNames: TokenNames,
        votingDuration: QuantizedFiniteDuration,
        tallyFeeAllowance: Coin,
    )

    /** Extract the transaction builder configuration from a [[DappLedgerM]]
      */
    private val ask: DappLedgerM[DappLedgerM.Config] =
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

    /** Check that a deposit tx parses correctly and add the deposit utxo it produces to the
      * ledger's state.
      *
      * NOTE: This checks SOME time bounds. Specifically, it checks whether the deposit's absorption
      * validity period ended prior to the start of the current block.
      */
    def registerDeposit(req: RegisterDeposit): DappLedgerM[Unit] = {
        import req.*
        for {
            config <- ask
            depositRefundTxSeqConfig = DepositRefundTxSeq.Config(
              txTiming = config.txTiming,
              cardanoInfo = config.cardanoInfo,
              headMultisigScript = config.headMultisigScript,
              multisigRegimeUtxo = config.multisigRegimeUtxo
            )
            parseRes =
                DepositRefundTxSeq
                    .parse(
                      depositTxBytes = depositTxBytes,
                      refundTxBytes = refundTxBytes,
                      config = depositRefundTxSeqConfig,
                      virtualOutputsBytes = virtualOutputsBytes,
                      donationToTreasury = donationToTreasury,
                    )
                    .left
                    .map(ParseError(_))
            depositRefundTxSeq <- lift(parseRes)
            depositProduced <- lift(
              if depositRefundTxSeq.depositTx.validityEnd
                      + txTiming.depositMaturityDuration
                      + txTiming.depositAbsorptionDuration < blockStartTime
              then Left(AbsorptionPeriodExpired(depositRefundTxSeq))
              else Right(depositRefundTxSeq.depositTx.depositProduced)
            )
            s <- get
            newState = s.appendToQueue((eventId, depositProduced))
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
        validDeposits: Queue[(LedgerEventId, DepositUtxo)],
        payoutObligations: Vector[Payout.Obligation],
        immatureDeposits: Queue[(LedgerEventId, DepositUtxo)],
        blockCreatedOn: QuantizedInstant,
        competingFallbackValidityStart: QuantizedInstant,
    ): DappLedgerM[SettleLedger.Result] = {

        for {
            config <- ask
            state <- get

            args = SettlementTxSeq.Builder.Args(
              kzgCommitment = nextKzg,
              majorVersionProduced =
                  BlockVersion.Major(state.treasury.datum.versionMajor.toInt + 1),
              treasuryToSpend = state.treasury,
              depositsToSpend = Vector.from(validDeposits.map(_._2).toList),
              payoutObligationsRemaining = payoutObligations,
              competingFallbackValidityStart = competingFallbackValidityStart,
              blockCreatedOn = blockCreatedOn,
            )

            settlementTxSeqRes <- lift(
              SettlementTxSeq
                  .Builder(
                    SettlementTxSeq.Config(
                      headMultisigScript = config.headMultisigScript,
                      multisigRegimeUtxo = config.multisigRegimeUtxo,
                      tokenNames = config.tokenNames,
                      votingDuration = config.votingDuration,
                      txTiming = config.txTiming,
                      tallyFeeAllowance = config.tallyFeeAllowance,
                      cardanoInfo = config.cardanoInfo
                    )
                  )
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
        blockCreatedOn: QuantizedInstant,
        competingFallbackValidityStart: QuantizedInstant,
    ): DappLedgerM[FinalizationTxSeq] = {
        for {
            s <- get
            config <- ask
            args = FinalizationTxSeq.Builder.Args(
              majorVersionProduced =
                  BlockVersion.Major(s.treasury.datum.versionMajor.toInt).increment,
              treasuryToSpend = s.treasury,
              payoutObligationsRemaining = payoutObligationsRemaining,
              competingFallbackValidityStart = competingFallbackValidityStart,
              blockCreatedOn = blockCreatedOn,
            )
            ftxSeq <- lift(
              FinalizationTxSeq
                  .Builder(
                    FinalizationTxSeq.Config(
                      txTiming = config.txTiming,
                      multisigRegimeUtxo = config.multisigRegimeUtxo,
                      cardanoInfo = config.cardanoInfo,
                      headMultisigScript = config.headMultisigScript,
                      equityShares = config.equityShares
                    )
                  )
                  .build(args)
                  .left
                  .map(Error.FinalizationTxSeqBuilderError.apply)
            )
        } yield ftxSeq
    }

    final case class State(
        treasury: MultisigTreasuryUtxo,
        // TODO: Queue[(EventId, DepositUtxo, RefundTx.PostDated)]
        deposits: Queue[(LedgerEventId, DepositUtxo)] = Queue()
    ) {
        def appendToQueue(t: (LedgerEventId, DepositUtxo)): State =
            this.copy(treasury, deposits.appended(t))

        def depositUtxoByEventId(ledgerEventId: LedgerEventId): Option[DepositUtxo] = ???
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

        final case class ParseError(wrapped: txseq.DepositRefundTxSeq.ParseError)
            extends RegisterDepositError

        /** This is raised during a call to `RegisterDeposit` if the deposit parses successfully,
          * but reveals an absorption window that is already prior to the block's start time. In
          * this case, the deposit will NOT be added to the dapp ledger's state.
          */
        // NOTE: I'm still returning the successfully-parsed TxSeq, but hesitantly... I'd usually prefer to not
        // have such a value in scope, but it seems like it might be important to error handling.
        // Don't do something dumb like pull this error from a Left and use the TxSeq as if it was valid
        final case class AbsorptionPeriodExpired(depositRefundTxSeq: DepositRefundTxSeq)
            extends RegisterDepositError

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
        ): IO[B] = {
            val dappLedgerMConfig: DappLedgerM.Config = DappLedgerM.Config(
              txTiming = jl.config.txTiming,
              equityShares = jl.config.equityShares,
              headMultisigScript = jl.config.headMultisigScript,
              multisigRegimeUtxo = jl.config.multisigRegimeUtxo,
              cardanoInfo = jl.config.cardanoInfo,
              tokenNames = jl.config.tokenNames,
              votingDuration = jl.config.votingDuration,
              tallyFeeAllowance = jl.config.tallyFeeAllowance
            )
            for {
                oldState <- jl.state.get
                res = action.run(dappLedgerMConfig, oldState.dappLedgerState)
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
}
