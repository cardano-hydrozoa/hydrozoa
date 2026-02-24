package hydrozoa.multisig.ledger

import cats.data.*
import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger
import hydrozoa.multisig.ledger.DappLedgerM.Error.{ParseError, SettlementTxSeqBuilderError, SubmissionPeriodIsOver}
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.dapp.txseq
import hydrozoa.multisig.ledger.dapp.txseq.{DepositRefundTxSeq, FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.event.LedgerEvent.DepositEvent
import hydrozoa.multisig.ledger.event.LedgerEventId
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import monocle.syntax.all.*
import scala.collection.immutable.Queue
import scala.math.Ordered.orderingToOrdered

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
    type Config = HeadConfig.Section

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
    def registerDeposit(
        req: DepositEvent,
        blockStartTime: QuantizedInstant
    ): DappLedgerM[Unit] = {
        import req.*
        for {
            config <- ask
            parseRes =
                DepositRefundTxSeq
                    .Parse(config)(
                      depositTxBytes = depositTxBytes,
                      refundTxBytes = refundTxBytes,
                      virtualOutputsBytes = virtualOutputsBytes
                    )
                    .result
                    .left
                    .map(ParseError(_))
            depositRefundTxSeq <- lift(parseRes)
            s <- get

            depositProduced <- lift(
              // TODO: add explicit vals to cope boolean blindness
              // Check that submission is still possible and if not - reject
              if depositRefundTxSeq.depositTx.validityEnd < blockStartTime
              then Left(SubmissionPeriodIsOver)

              // TODO: I believe we should remove it
              // Check absorption is still possible
              // The previous check is stronger I think
              // else if config.txTiming.depositAbsorptionEndTime(
              //      depositRefundTxSeq.depositTx.validityEnd
              //    ) < blockStartTime
              // then Left(AbsorptionPeriodExpired(depositRefundTxSeq))
              else Right(depositRefundTxSeq.depositTx.depositProduced)
            )
            newState = s.appendToQueue((eventId, depositProduced))
            _ <- set(newState)
        } yield ()
    }

    /** Construct settlement tx seq and set the new treasury to the state.
      *
      * The collective value of the [[payouts]] must '''not''' exceed the [[treasury]] value.
      */
    def mkSettlementTxSeq(
        nextKzg: KzgCommitment,
        absorbedDeposits: Queue[(LedgerEventId, DepositUtxo)],
        payoutObligations: Vector[Payout.Obligation],
        blockCreatedOn: QuantizedInstant,
        competingFallbackValidityStart: QuantizedInstant,
    ): DappLedgerM[SettlementTxSeq] = {

        for {
            config <- ask
            state <- get

            majorVersionProduced = BlockVersion.Major(state.treasury.datum.versionMajor.toInt + 1)

            // TODO: add logger
            // _ = println(
            //  s"DappLedgerM.settleLedger: state.treasury.datum.versionMajor=${state.treasury.datum.versionMajor}"
            // )
            // _ = println(s"DappLedgerM.settleLedger: majorVersionProduced=${majorVersionProduced}")

            settlementTxSeq <- lift(
              SettlementTxSeq
                  .Build(config)(
                    kzgCommitment = nextKzg,
                    majorVersionProduced = majorVersionProduced,
                    treasuryToSpend = state.treasury,
                    depositsToSpend = Vector.from(absorbedDeposits.map(_._2).toList),
                    payoutObligationsRemaining = payoutObligations,
                    competingFallbackValidityStart = competingFallbackValidityStart,
                    blockCreatedOn = blockCreatedOn
                  )
                  .result
                  .left
                  .map(SettlementTxSeqBuilderError.apply)
            )

            newState = state.copy(
              treasury = settlementTxSeq.settlementTx.treasuryProduced
            )
            _ <- set(newState)

        } yield settlementTxSeq

    }

    /** Remove the absorbed/refunded deposits and update the treasury in the ledger state. Called
      * when the block weaver sends the single to close the block in leader mode.
      *
      * @param ineligibleDeposits
      * @return
      */
    def handleBlockBrief(
        ineligibleDeposits: Queue[(LedgerEventId, DepositUtxo)],
    ): DappLedgerM[Unit] = for {
        state <- get

        newState = state.copy(
          deposits = ineligibleDeposits
        )
        _ <- set(newState)
    } yield ()

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
            ftxSeq <- lift(
              FinalizationTxSeq
                  .Build(config)(
                    majorVersionProduced =
                        BlockVersion.Major(s.treasury.datum.versionMajor.toInt).increment,
                    treasuryToSpend = s.treasury,
                    payoutObligationsRemaining = payoutObligationsRemaining,
                    competingFallbackValidityStart = competingFallbackValidityStart,
                    blockCreatedOn = blockCreatedOn,
                  )
                  .result
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

    sealed trait Error
    object Error {

        sealed trait RegisterDepositError extends DappLedgerM.Error

        final case class ParseError(wrapped: txseq.DepositRefundTxSeq.Parse.Error)
            extends RegisterDepositError

        case object SubmissionPeriodIsOver extends RegisterDepositError

        /** This is raised during a call to `RegisterDeposit` if the deposit parses successfully,
          * but reveals an absorption window that is already prior to the block's start time. In
          * this case, the deposit will NOT be added to the dapp ledger's state.
          */
        // NOTE: I'm still returning the successfully-parsed TxSeq, but hesitantly... I'd usually prefer to not
        // have such a value in scope, but it seems like it might be important to error handling.
        // Don't do something dumb like pull this error from a Left and use the TxSeq as if it was valid
        final case class AbsorptionPeriodExpired(depositRefundTxSeq: DepositRefundTxSeq)
            extends RegisterDepositError

        final case class SettlementTxSeqBuilderError(wrapped: SettlementTxSeq.Build.Error)
            extends DappLedgerM.Error

        final case class FinalizationTxSeqBuilderError(wrapped: FinalizationTxSeq.Build.Error)
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
            for {
                oldState <- jl.state.get
                _ <- IO.println(
                  s"[runDappLedgerM] Before action: oldState.dappLedgerState.treasury.datum.versionMajor=${oldState.dappLedgerState.treasury.datum.versionMajor}"
                )
                res = action.run(jl.config, oldState.dappLedgerState)
                b <- res match {
                    case Left(error) => onFailure(error)
                    case Right(newState, a) =>
                        for {
                            _ <- IO.println(
                              s"[runDappLedgerM] After action: newState.treasury.datum.versionMajor=${newState.treasury.datum.versionMajor}"
                            )
                            _ <- jl.state.set(oldState match {
                                case d: JointLedger.Done =>
                                    d.focus(_.dappLedgerState).replace(newState)
                                case p: JointLedger.Producing =>
                                    p.focus(_.dappLedgerState).replace(newState)
                            })
                            _ <- IO.println("[runDappLedgerM] State updated in JointLedger")
                            b <- onSuccess(a)
                        } yield b
                }
            } yield b
        }
    }
}
