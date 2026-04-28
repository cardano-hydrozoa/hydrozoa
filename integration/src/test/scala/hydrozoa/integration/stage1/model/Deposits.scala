package hydrozoa.integration.stage1.model

import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.integration.stage1.Commands.RegisterDepositCommand
import hydrozoa.integration.stage1.Model

import scala.collection.immutable.Queue
import monocle.*
import monocle.syntax.all.*
import scalus.|>
import cats.data.{Kleisli, RWS, State}
import cats.*
import cats.syntax.all.*
import Deposits.DepositStatus
import Deposits.DepositStatus.*
import scalus.cardano.ledger.{Transaction, TransactionInput}

/** A state machine for Deposits. Each deposit can only be in one state at a time. Possible transitions are:
  *
  *                    _____________________________________
  *                    |                                   |
  *   Enqueued -> Registered -> Submitted -> Absorbed     |
  *       |           |                |                  |
  *       v           v                v                  |
  *     Rejected     Declined -> Refunded <---------------|
  *
  *     Note that the [[Model.State]] is "omnipresent" (all-seeing) and "omnipotent" (all-doing). In this way, it can
  *     "see" more information than any given party to the hydrozoa protocol (including end-users, cardano nodes, and
  *     hyrdozoa node peers) and simulate actions on their behalf.
  *
  *     This has certain ramifications for how the [[Model.State]] (and in particular, the [[Deposits]] type) corresponds
  *     to the "view" seen by the SUT (a hydrozoa node):
  *
  *     - A hydrozoa node itself "at rest" only knows about Rejected, Registered, Refunded, or Absorbed deposits
  *     - "In transit" the node knows about:
  *       - Enqueued, via the accumulation of all web service requests received and not yet processed
  *       - Declined, via the registered deposits that mature but who's deposit utxo isn't found during block creation
  *         in the joint ledger
  *       - Submitted, via the registered deposits that mature and ARE found during block creation in the joint ledger
  *
  *     The upshot is that when constructing something like the [[MajorBlockWakeupTime]], this must be calculated
  *     according to _not only_ the [[Registered]] deposits, but to the [[Declined]] and [[Submitted]] as well.
  */

object Deposits {
    enum DepositStatus private (cmd: RegisterDepositCommand) {
        // The queue of all generated deposits that Alice wants to register.
        // At all times, all deposits in the list are disjoint in terms of their funding utxo, see [[utxosLocked]].
        case Enqueued private (cmd: RegisterDepositCommand) extends DepositStatus(cmd)
        // A deposits registered by Hydrozoa, i.e. included in a block brief with positive validity flag.
        case Registered private (cmd: RegisterDepositCommand) extends DepositStatus(cmd)
        // After a deposit was registered, we may submit it or cancel it depending on
        // how much time is left until the deposit tx's TTL is up - I call it runway.
        // Upon generating [[SubmitDepositsCommand]] we assess whether we have enough
        // runway to take off the deposit - i.e. how much time we have from now to the
        // ttl. This is needed, because the test fails if SUT can't submit deposit tx
        // that model expects to see.
        //
        // So we have two partitions here:
        //  - deposits that have been submitted, so they are expected to appear in the
        // very first block that satisfies their absorption window
        case Submitted private (cmd: RegisterDepositCommand) extends DepositStatus(cmd)
        //  - deposits, that the model decided not to submit - their funding utxos get
        // unlocked so they can be reused
        case Declined private (cmd: RegisterDepositCommand) extends DepositStatus(cmd)
        // Deposits that were submitted and marked for absorption by the model
        case Absorbed private (cmd: RegisterDepositCommand) extends DepositStatus(cmd)
        case Rejected private (cmd: RegisterDepositCommand) extends DepositStatus(cmd)
        case Refunded private (cmd: RegisterDepositCommand) extends DepositStatus(cmd)

        export cmd.*
    }

    // These are the absorption times as seen by hydrozoa, which doesn't have a priori knowledge of whether
    // a registered deposit as already been submitted or declined by the model (acting in the role of "end-user")
    extension (hydrozoaRegistered: Refundable) {
        def depositAbsorptionStart(using config: TxTiming.Section): DepositAbsorptionStartTime =
            val requestValidityEnd = hydrozoaRegistered.request.request.header.validityEnd
            config.txTiming.depositAbsorptionStartTime(requestValidityEnd)

        def depositAbsorptionEnd(using config: TxTiming.Section): DepositAbsorptionEndTime =
            val requestValidityEnd = hydrozoaRegistered.request.request.header.validityEnd
            config.txTiming.depositAbsorptionEndTime(requestValidityEnd)

        def cmd: RegisterDepositCommand = hydrozoaRegistered match {
            case x: Registered => x.cmd
            case y: Declined   => y.cmd
            case z: Submitted  => z.cmd
        }

    }

    /** Implementation note: the companion object for DepositStatus is implemented such that there
      * is no public way to construct something like `DepositStatus.Registered` directly; you _must_
      * pass it through Registered.register and update the Model.State accordingly
      */
    object DepositStatus {
        type Refundable = Registered | Submitted | Declined
        object Enqueued {

            /** Enqueue incoming commands.
              *
              * @return
              */
            def enqueue(cmds: Queue[RegisterDepositCommand]): (Model.State => Model.State) =
                deposits =>
                    val enqueued: Queue[Enqueued] = cmds.map(Enqueued(_))
                    deposits
                        .focus(_.deposits.fullQueue)
                        .modify(_ ++ enqueued)
                        .focus(_.deposits.depositsEnqueued)
                        .modify(_ ++ enqueued)
        }

        object Registered {
            def register: AppendDepositM[DepositStatus.Enqueued, DepositStatus.Registered] =
                transformationHelper(
                  enqueued => new Registered(enqueued.cmd),
                  depositsEnqueuedL,
                  depositsRegisteredL
                )
        }

        object Submitted {
            def submit: AppendDepositM[DepositStatus.Registered, DepositStatus.Submitted] =
                toSubmit =>
                    for {
                        submitted <- transformationHelper(
                          (registered: Registered) => new Submitted(registered.cmd),
                          depositsRegisteredL,
                          depositsSubmittedL
                        )(toSubmit)

                        _ <- submitted.traverse(submitted =>
                            State[Model.State, Unit](oldState =>
                                (
                                  oldState.applyContinuingL1Tx(submitted.cmd.depositTxBytesSigned),
                                  ()
                                )
                            )
                        )
                    } yield submitted
        }

        object Declined {
            def decline: AppendDepositM[DepositStatus.Registered, DepositStatus.Declined] =
                toDecline => {
                    for {
                        declined <- transformationHelper(
                          (registered: Registered) => new Declined(registered.cmd),
                          depositsRegisteredL,
                          depositsDeclinedL
                        )(toDecline)

                        inputsToUnlock = declined
                            .map(_.cmd.depositTxBytesSigned.body.value.inputs.toSet)
                            .foldLeft(Set.empty[TransactionInput])(_ ++ _)

                        _ <- State[Model.State, Unit](original =>
                            (original.focus(_.utxoLocked).modify(_ -- inputsToUnlock), ())
                        )
                    } yield declined
                }
        }

        object Absorbed {
            def absorb: AppendDepositM[DepositStatus.Submitted, DepositStatus.Absorbed] =
                transformationHelper(
                  submitted => Absorbed(submitted.cmd),
                  depositsSubmittedL,
                  depositsAbsorbedL
                )
        }

        object Rejected {

            def reject: AppendDepositM[DepositStatus.Enqueued, DepositStatus.Rejected] =
                transformationHelper(
                  enqueued => new Rejected(enqueued.cmd),
                  depositsEnqueuedL,
                  depositsRejectedL
                )
        }

        object Refunded {
            // This is a little bit ugly...
            def refund: AppendDepositM[Refundable, Refunded] = toAppend => {
                // helper alias to run the transformationHelper and accumulate the refunded deposits at each step
                type HelperM = RWS[Unit, Queue[DepositStatus.Refunded], Model.State, Unit]

                // lift the result of transformationHelper into the RWS
                def liftState(stateM: State[Model.State, Queue[Refunded]]): HelperM =
                    RWS((_, oldState) =>
                        val (newState, newlyRefunded) = stateM.run(oldState).value
                        (newlyRefunded, newState, ())
                    )

                // Build up a sequence of computations that:
                // - Modify the Model.State
                // - Indicate which deposits were refunded
                val helperComputation: Queue[HelperM] =
                    toAppend.map {
                        case registered: Registered =>
                            transformationHelper[Registered, Refunded](
                              refundable => new Refunded(refundable.cmd),
                              depositsRegisteredL,
                              depositsRefundedL
                            )(
                              Queue(registered)
                            ) |> liftState
                        case declined: Declined =>
                            transformationHelper[Declined, Refunded](
                              refundable => new Refunded(refundable.cmd),
                              depositsDeclinedL,
                              depositsRefundedL
                            )(
                              Queue(declined)
                            ) |> liftState
                        case submitted: Submitted =>
                            transformationHelper[Submitted, Refunded](
                              refundable => new Refunded(refundable.cmd),
                              depositsSubmittedL,
                              depositsRefundedL
                            )(
                              Queue(submitted)
                            ) |> liftState

                    }

                // Sequence the computations, run the RWS, and lower it into `State`
                cats.data.State(oldState =>
                    helperComputation.sequence_
                        .run((), oldState)
                        .map((refunded, state, _) => (state, refunded))
                        .value
                )
            }
        }

    }

    val empty: Deposits = new Deposits()

    private def depositsEnqueuedL: Lens[Model.State, Queue[Enqueued]] =
        Focus[Model.State](_.deposits.depositsEnqueued)

    private def depositsRegisteredL: Lens[Model.State, Queue[Registered]] =
        Focus[Model.State](_.deposits.depositsRegistered)

    private def depositsSubmittedL: Lens[Model.State, Queue[Submitted]] =
        Focus[Model.State](_.deposits.depositsSubmitted)

    private def depositsDeclinedL: Lens[Model.State, Queue[Declined]] =
        Focus[Model.State](_.deposits.depositsDeclined)

    private def depositsAbsorbedL: Lens[Model.State, Queue[Absorbed]] =
        Focus[Model.State](_.deposits.depositsAbsorbed)

    private def depositsRejectedL: Lens[Model.State, Queue[Rejected]] =
        Focus[Model.State](_.deposits.depositsRejected)

    private def depositsRefundedL: Lens[Model.State, Queue[Refunded]] =
        Focus[Model.State](_.deposits.depositsRefunded)

    private type AppendDepositM[From <: DepositStatus, To <: DepositStatus] =
        Queue[From] => cats.data.State[Model.State, Queue[To]]

    import DepositStatus.*

    /** A utility helper to update the full queue (preserving ordering), append to a new queue, and
      * remove from the old queue
      *
      * @param f
      *   a function to transform the deposit status. It should be one of the "apply" methods of the
      *   DepositStatus enum
      * @param fromQueue
      *   the (mutable) LinkedHashSet to remove elements from
      * @param toQueue
      *   the (mutable) LinkedHashSet to add elements to
      * @param toAppend
      *   the (immutable) queue of deposits to append to the "toQueue"
      * @tparam From
      *   the type of queue we are removing from
      * @tparam To
      *   the type of queue we are adding to
      */
    private def transformationHelper[From <: DepositStatus, To <: DepositStatus](
        f: From => To,
        fromQueueLens: Lens[Model.State, Queue[From]],
        toQueueLens: Lens[Model.State, Queue[To]]
    ): AppendDepositM[From, To] = toAppend => {

        // Updates the full Queue with new deposit states
        val transformFullQueue: (Model.State => Model.State) = original =>
            original
                .focus(_.deposits.fullQueue)
                .modify(
                  _.map(deposit =>
                      if toAppend.map(_.request.requestId).contains(deposit.request.requestId)
                      then f(deposit.asInstanceOf[From])
                      else deposit
                  )
                )

        // Removes the items that we will append to the _other_ Queue from this Queue
        val removeFromQueue: (Model.State => Model.State) = original =>
            original |> fromQueueLens.modify(
              _.filterNot(deposit =>
                  toAppend.map(_.request.requestId).contains(deposit.request.requestId)
              )
            )

        val addToQueue: (Model.State => Model.State) = original =>
            original |> toQueueLens.modify(_.appendedAll(toAppend.map(f)))

        cats.data.State(state =>
            (state |> transformFullQueue |> removeFromQueue |> addToQueue, toAppend.map(f))
        )
    }

}

// Implementation notes:
// - I'm using multiple Queues for performance, so that we don't have to filter the full queue to get the Queues we care
//   about, or assemble the individual Queues to get the full Queue
case class Deposits private (
    fullQueue: Queue[DepositStatus] = Queue.empty,
    depositsEnqueued: Queue[Enqueued] = Queue.empty,
    depositsRegistered: Queue[Registered] = Queue.empty,
    depositsSubmitted: Queue[Submitted] = Queue.empty,
    depositsDeclined: Queue[Declined] = Queue.empty,
    depositsAbsorbed: Queue[Absorbed] = Queue.empty,
    depositsRejected: Queue[Rejected] = Queue.empty,
    depositsRefunded: Queue[Refunded] = Queue.empty
) {

    /** All the registered deposits that hydrozoa "knows" about at rest
      */
    def hydrozoaKnownRegisteredDeposits: Queue[Registered | Declined | Submitted] =
        (depositsRegistered ++ depositsSubmitted ++ depositsDeclined).map(
          _.asInstanceOf[Registered | Declined | Submitted]
        )
}
