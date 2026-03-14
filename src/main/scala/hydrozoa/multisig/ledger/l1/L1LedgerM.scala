package hydrozoa.multisig.ledger.l1

import cats.data.*
import cats.syntax.all.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, FallbackTxStartTime}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.{DepositAbsorptionStartTime, RequestValidityEndTime}
import hydrozoa.config.head.multisig.timing.{TxTiming, given_Ordering_DepositAbsorptionStartTime}
import hydrozoa.lib.cardano.scalus.QuantizedTime.toEpochQuantizedInstant
import hydrozoa.multisig.ledger
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l1.L1LedgerM.Error.{DepositTxInvalidTTL, ParseError, SettlementTxSeqBuilderError}
import hydrozoa.multisig.ledger.l1.tx.RefundTx
import hydrozoa.multisig.ledger.l1.txseq.{DepositRefundTxSeq, FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.l1.utxo.{DepositUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.server.UserRequestWithId
import scala.collection.immutable.{Queue, TreeMap}

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

    private def map[B](f: A => B): L1LedgerM[B] = L1LedgerM(this.unL1LedgerM.map(f))

    private def flatMap[B](f: A => L1LedgerM[B]): L1LedgerM[B] =
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
            requestValidityEndTime = RequestValidityEndTime(
              header.validityEnd.toEpochQuantizedInstant(config.slotConfig)
            )
            headerSubmissionDeadline = config.txTiming.depositSubmissionDeadline(
              requestValidityEndTime
            )

            parseRes =
                DepositRefundTxSeq
                    .Parse(config)(
                      depositTxBytes = body.l1Payload,
                      l2Payload = body.l2Payload,
                      requestId = requestWithId.requestId,
                      requestValidityEndTime = requestValidityEndTime
                    )
                    .result
                    .left
                    .map(ParseError(_))
            depositRefundTxSeq <- lift(parseRes)

            depositProduced <- lift(
              if depositRefundTxSeq.depositTx.submissionDeadline == headerSubmissionDeadline
              then Right(depositRefundTxSeq.depositTx.depositProduced)
              else Left(DepositTxInvalidTTL)
            )

            s <- get
            updatedMap <- s.deposits.appended((requestId, depositProduced))
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
        absorbedDeposits: Queue[(RequestId, DepositUtxo)],
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
                    depositsToSpend = Vector.from(absorbedDeposits.map(_._2).toList),
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

    /** Remove the absorbed/refunded deposits and update the treasury in the ledger state. Called
      * when the block weaver sends the single to close the block in leader mode.
      * @return
      */
    def handleBlockBrief(
        ineligibleDeposits: DepositsMap,
    ): L1LedgerM[Unit] = for {
        state <- get

        newState = state.copy(
          deposits = ineligibleDeposits
        )
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

    /** deposits in a TreeMap according to their absorption start time. The Tree map ensures that
      * the traversal order is according to the absorption start time, with ties being broken
      * according to the total ordering of the ledger events, such that each queue in this map is a
      * subsequence of the totally-ordered event stream.
      */
    final case class DepositsMap private (
        treeMap: TreeMap[DepositAbsorptionStartTime, Queue[(RequestId, DepositUtxo)]]
    ) {

        /** Append an event to the end of the queue of events with the same start time.
          */
        def appended(event: (RequestId, DepositUtxo)): L1LedgerM[DepositsMap] = {
            for {
                config <- ask
                res = this.appended(event, config.txTiming)
            } yield res
        }

        /** Append an event to the end of the queue of events with the same start time.
          */
        def appended(event: (RequestId, DepositUtxo), txTiming: TxTiming): DepositsMap = {
            val absorptionStartTime =
                txTiming.depositAbsorptionStartTime(event._2.requestValidityEndTime)
            DepositsMap(this.treeMap.updatedWith(absorptionStartTime) {
                case None        => Some(Queue(event))
                case Some(queue) => Some(queue.appended(event))
            })
        }

        def ++(otherMap: DepositsMap): DepositsMap =
            DepositsMap(this.treeMap ++ otherMap.treeMap)

        def splitAt(n: Int): (DepositsMap, DepositsMap) = {
            val (p1, p2) = this.treeMap.splitAt(n)
            (DepositsMap(p1), DepositsMap(p2))
        }

        def foldLeft[Acc](acc: Acc)(
            f: (Acc, (DepositAbsorptionStartTime, Queue[(RequestId, DepositUtxo)])) => Acc
        ): Acc =
            treeMap.foldLeft(acc)(f)

        lazy val isEmpty: Boolean = numberOfDeposits == 0

        lazy val numberOfDeposits: Int = flatValues.size

        /** Event-deposit tuples traversed in order of absorption start time, with ties broken
          * according to the order in which they were added to the DepositsMap (which should
          * correspond to the total order of the event stream)
          */
        lazy val flatValues: Iterable[(RequestId, DepositUtxo)] = treeMap.values.flatten

        /** Event IDs traversed in order of the absorption start time associated with the
          * corresponding ddeposit, with ties broken according to the order in which they were added
          * to the DepositsMap (which should correspond to the total order of the event stream)
          */
        lazy val flatEvents: Iterable[RequestId] = flatValues.map(_._1)

        /** Deposits traversed in order of absorption start time, with ties broken according to the
          * order in which they were added to the DepositsMap (which should correspond to the total
          * order of the event stream)
          */
        lazy val flatDeposits: Iterable[DepositUtxo] = flatValues.map(_._2)
    }

    // TODO: Make a constructor method taking variadic args, or _.from method?
    object DepositsMap:
        def empty: DepositsMap = DepositsMap(
          TreeMap.empty[DepositAbsorptionStartTime, Queue[(RequestId, DepositUtxo)]]
        )

    sealed trait Error extends Throwable
    object Error {

        sealed trait RegisterDepositError extends L1LedgerM.Error

        final case class ParseError(wrapped: txseq.DepositRefundTxSeq.Parse.Error)
            extends RegisterDepositError

        case object DepositTxInvalidTTL extends RegisterDepositError

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
            extends L1LedgerM.Error

        final case class FinalizationTxSeqBuilderError(wrapped: FinalizationTxSeq.Build.Error)
            extends L1LedgerM.Error
    }
}
