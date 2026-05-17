package hydrozoa.multisig.ledger.l1

import cats.data.*
import cats.syntax.all.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, FallbackTxStartTime}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.RequestValidityEndTime
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.ledger
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l1.L1LedgerM.Error.{DepositTxInvalidTTL, ParseError, SettlementTxSeqBuilderError}
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap
import hydrozoa.multisig.ledger.l1.tx.RefundTx
import hydrozoa.multisig.ledger.l1.txseq.{DepositRefundTxSeq, FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.l1.utxo.{DepositUtxo, MultisigTreasuryUtxo}

private type E[A] = Either[L1LedgerM.Error, A]
private type S[A] = cats.data.StateT[E, L1LedgerM.State, A]
private type RT[A] = ReaderT[S, L1LedgerM.Config, A]

/** L1LedgerM defines an opaque monad stack for manipulating the [[State]] of the L1 ledger as seen
  * by the Joint Ledger. It's constructor and eliminator methods are private so that it cannot be
  * used in unintended ways.
  *
  * See the companion object for details on allowed operations.
  */
case class L1LedgerM[A] private (private val unL1LedgerM: RT[A]) {

    import L1LedgerM.*

    def map[B](f: A => B): L1LedgerM[B] = L1LedgerM(this.unL1LedgerM.map(f))

    def flatMap[B](f: A => L1LedgerM[B]): L1LedgerM[B] =
        L1LedgerM(this.unL1LedgerM.flatMap(a => f(a).unL1LedgerM))

    /** Use [[runL1LedgerM()]] instead
      *
      * @return
      */
    def run(
        config: L1LedgerM.Config,
        initialState: L1LedgerM.State
    ): Either[L1LedgerM.Error, (L1LedgerM.State, A)] =
        this.unL1LedgerM.run(config).run(initialState)

}

object L1LedgerM {
    type Config = HeadConfig.Section

    /** Extract the transaction builder configuration from a [[L1LedgerM]]
      */
    private val ask: L1LedgerM[L1LedgerM.Config] =
        L1LedgerM(Kleisli.ask)

    /** Obtain the current state from a [[L1LedgerM]]
      */
    private val get: L1LedgerM[L1LedgerM.State] =
        L1LedgerM(Kleisli.liftF(cats.data.StateT.get))

    /** Set the state of the [[L1LedgerM]] state machine
      */
    private def set(newState: L1LedgerM.State): L1LedgerM[Unit] =
        L1LedgerM(Kleisli.liftF(cats.data.StateT.set(newState)))

    /** Lift an [[Either]] into a [[L1LedgerM]].
      */
    private def lift[A](e: Either[L1LedgerM.Error, A]): L1LedgerM[A] =
        L1LedgerM(Kleisli.liftF(StateT.liftF(e)))

    /** Inject a pure value into [[L1LedgerM]]. Useful when an effect-derivation path produces a
      * value without needing to read or mutate the L1 state (e.g. minor-only stacks whose only
      * effect is a list of pre-built refund txs).
      */
    def pure[A](a: A): L1LedgerM[A] = lift(Right(a))

    /** Cats Monad instance enabling `traverse` and friends. Implements `tailRecM` via the
      * StateT/ReaderT stack already used internally.
      */
    given cats.Monad[L1LedgerM] with {
        override def pure[A](a: A): L1LedgerM[A] = L1LedgerM.pure(a)

        override def flatMap[A, B](fa: L1LedgerM[A])(f: A => L1LedgerM[B]): L1LedgerM[B] =
            fa.flatMap(f)

        override def tailRecM[A, B](a: A)(f: A => L1LedgerM[Either[A, B]]): L1LedgerM[B] =
            L1LedgerM(cats.Monad[RT].tailRecM(a)(a0 => f(a0).unL1LedgerM))
    }

    /** Check that a deposit tx parses correctly and add the deposit utxo it produces to the
      * ledger's state.
      *
      * NOTE: This checks SOME time bounds. Specifically, it checks whether the deposit's absorption
      * validity period ended prior to the start of the current block.
      */
    def registerDeposit(
        requestWithId: UserRequestWithId.DepositRequest
    ): L1LedgerM[(DepositUtxo, RefundTx.PostDated)] = {
        import requestWithId.*
        import request.*
        for {
            config <- ask
            validityEndTime = RequestValidityEndTime(header.validityEnd)

            parseRes =
                DepositRefundTxSeq
                    .Parse(config)(
                      depositTxBytes = body.l1Payload,
                      l2Payload = body.l2Payload,
                      requestId = requestWithId.requestId,
                      requestValidityEndTime = validityEndTime
                    )
                    .result
                    .left
                    .map(ParseError(_))
            depositRefundTxSeq <- lift(parseRes)

            expectedSubmissionDeadline = config.txTiming.depositSubmissionDeadline(validityEndTime)

            depositProduced <- lift(
              if depositRefundTxSeq.depositTx.submissionDeadline == expectedSubmissionDeadline
              then Right(depositRefundTxSeq.depositTx.depositProduced)
              else
                  Left(
                    DepositTxInvalidTTL(
                      expectedSubmissionDeadline,
                      depositRefundTxSeq.depositTx.submissionDeadline
                    )
                  )
            )

            s <- get
            updatedMap = s.deposits.append(DepositsMap.Entry(requestId, depositProduced))
            newState = s.copy(deposits = updatedMap)
            _ <- set(newState)
        } yield (depositProduced, depositRefundTxSeq.refundTx)
    }

    /** Construct settlement tx seq and set the new treasury to the state.
      *
      * The collective value of the [[payouts]] must '''not''' exceed the [[treasury]] value.
      */
    def mkSettlementTxSeq(
        nextKzg: KzgCommitment,
        absorbedDeposits: List[DepositUtxo],
        payoutObligations: Vector[Payout.Obligation],
        blockCreationEndTime: BlockCreationEndTime,
        competingFallbackValidityStart: FallbackTxStartTime,
    ): L1LedgerM[SettlementTxSeq] = {

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
                    depositsToSpend = absorbedDeposits,
                    payoutObligationsRemaining = payoutObligations,
                    competingFallbackValidityStart = competingFallbackValidityStart,
                    blockCreationEndTime = blockCreationEndTime
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

    // NOTE: there is intentionally NO `mkStandaloneEvacCommitTx` here. A standalone
    // evacuation commitment is NOT a transaction and does NOT touch the L1 ledger / treasury:
    // it is a contingent, dormant record `(headId, blockVersion, kzgCommitment)` presented to
    // the L1 dispute-resolution scripts in the rules-based regime (only after a fallback
    // executes). The treasury's KZG advances on L1 only via settlement (major) / finalization
    // (final). The slow side builds the record purely in `StackEffectsBuilder` — see
    // `hydrozoa.multisig.ledger.l1.tx.StandaloneEvacuationCommitment`.

    /** Remove the absorbed/refunded deposits and update the treasury in the ledger state. Called
      * when the block weaver sends the single to close the block in leader mode.
      * @return
      */
    def handleBlockBrief(survivingDeposits: DepositsMap): L1LedgerM[Unit] = for {
        state <- get
        newState = state.copy(deposits = survivingDeposits)
        _ <- set(newState)
    } yield ()

    /** Construct a finalization transaction, a list of rollout transactions, and a list of
      * immediate refund transactions based on the arguments. The [[L1LedgerM]] must be discarded
      * after this, so there's no point in updating its state.
      *
      * The collective value of the [[payouts]] must '''not''' exceed the [[treasury]] value.
      * Immediate refund transactions must be constructed for every deposit in the ledger state.
      */
    // TODO (fund14): add Refund.Immediates to the return type
    def finalizeLedger(
        payoutObligationsRemaining: Vector[Payout.Obligation],
        competingFallbackValidityStart: FallbackTxStartTime,
    ): L1LedgerM[FinalizationTxSeq] = {
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
                    competingFallbackValidityStart = competingFallbackValidityStart
                  )
                  .result
                  .left
                  .map(Error.FinalizationTxSeqBuilderError.apply)
            )
        } yield ftxSeq
    }

    final case class State(
        treasury: MultisigTreasuryUtxo,
        deposits: DepositsMap
    )

    sealed trait Error extends Throwable
    object Error {

        sealed trait RegisterDepositError extends L1LedgerM.Error

//        final case class ParseError(wrapped: txseq.DepositRefundTxSeq.Parse.Error)
//            extends RegisterDepositError

        final case class ParseError(wrapped: txseq.DepositRefundTxSeq.Parse.Error)
            extends RegisterDepositError {
            override def toString: String = s"ParseError: $wrapped"
        }

        case class DepositTxInvalidTTL(
            expectedSubmissionDeadline: QuantizedInstant,
            actualSubmissionDeadline: QuantizedInstant
        ) extends RegisterDepositError {
            override def toString: String =
                s"DepositTxInvalidTTL: expected submission deadline $expectedSubmissionDeadline, but got $actualSubmissionDeadline"
        }

        final case class SettlementTxSeqBuilderError(wrapped: SettlementTxSeq.Build.Error)
            extends L1LedgerM.Error {
            override def toString: String =
                "Settlement tx-seq error:" + "\n" +
                    s"${wrapped.toString}"
        }

        final case class FinalizationTxSeqBuilderError(wrapped: FinalizationTxSeq.Build.Error)
            extends L1LedgerM.Error {
            override def toString: String =
                "Finalization tx-seq error:" + "\n" +
                    s"${wrapped.toString}"
        }
    }
}
