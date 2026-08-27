package hydrozoa.multisig.ledger.l2

import cats.Monad
import cats.data.EitherT
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.joint.{EvacuationDiff, EvacuationDiffGroup, EvacuationMapHash}
import scalus.cardano.ledger.Hash32

/** Why [[L2Ledger.restoreTo]] could not reconstruct the committed state. Extends `Throwable` so the
  * one fatal caller (`JointLedger.State.recover`, at boot) can raise it directly; it is still a
  * typed `Either` value the caller matches on, never thrown from `restoreTo` itself.
  */
sealed trait RestoreError extends RuntimeException

object RestoreError:
    /** The requested command number is beyond the ledger's durable tip — a corruption tripwire (the
      * co-anchoring ordering normally prevents it).
      */
    final case class CommandNumberTooHigh(requested: L2CommandNumber, tip: L2CommandNumber)
        extends RestoreError

    /** Replaying the logged commands failed — a ledger bug or store corruption. Named to mirror
      * [[L2LedgerResponse.UnrecoverableError.OtherError]], the command-path counterpart.
      */
    final case class OtherError(message: String) extends RestoreError

    /** The ledger's evacuation map at the restored command number is not the one this node holds.
      *
      * At a cold start that means the head config's `initialEvacuationMap` is not the map the L2
      * ledger actually starts from — the head was built against a different ledger, or against a
      * different configuration of this one. The initialization transaction has already committed to
      * the configured map on L1, so this cannot be repaired at runtime and must not be run through:
      * the head would work normally for its whole life and produce unevacuable payouts at fallback.
      * See `docs/spec/l2-ledger-command-coordination.md`.
      */
    final case class EvacuationMapMismatch(
        expected: EvacuationMapHash,
        actual: EvacuationMapHash
    ) extends RestoreError {
        override def getMessage: String =
            s"L2 ledger evacuation map digest $actual does not match the configured $expected"
    }

    /** The ledger's agreed parameters are not the ones this node's head config pins.
      *
      * `l2ParamsHash` never moves, so this is comparable at every anchor, warm or cold, against a
      * config value that is equally fixed. A mismatch means this node is driving a ledger the head
      * was not built against — not repairable at runtime, for the same reason
      * [[EvacuationMapMismatch]] is not.
      */
    final case class L2ParamsMismatch(
        expected: Hash32,
        actual: Hash32
    ) extends RestoreError {
        override def getMessage: String =
            s"L2 ledger parameters digest ${actual.toHex} does not match the configured " +
                s"${expected.toHex}"
    }

/** State changes accumulated via interaction with the L2 Ledger (i.e., as seen from the Joint
  * Ledger).
  *
  * @param diffs
  *   Evacuation diffs generated from [[L2LedgerCommand.ApplyDepositDecisions]]s and
  *   [[L2LedgerCommand.ApplyTransaction]], one [[EvacuationDiffGroup]] per applied command in
  *   command order — the per-command boundary the slow side's value-conservation check needs.
  *
  * @param payouts
  *   Payouts generated from [[L2LedgerCommand.ApplyTransaction]]
  *
  * @param payoutRequestIds
  *   The producing request of each entry in `payouts`, in the same order (all payouts of one
  *   `ApplyTransaction` share its `requestId`). Local-only provenance for withdrawal-effect
  *   tracking; never on the wire or on-chain.
  */
final case class L2LedgerInteractionState private (
    diffs: Vector[EvacuationDiffGroup],
    payouts: Vector[Payout.Obligation],
    payoutRequestIds: Vector[RequestId]
):
    /** Fold in an [[L2LedgerResponse.Applied.ApplyDepositDecisions]]'s effects: append its
      * evacuation diffs as the block's [[EvacuationDiffGroup.DepositDecisions]] group.
      */
    def appendDecisionEffects(extraDiffs: Vector[EvacuationDiff]): L2LedgerInteractionState =
        L2LedgerInteractionState(
          diffs :+ EvacuationDiffGroup.DepositDecisions(extraDiffs),
          payouts,
          payoutRequestIds
        )

    /** Fold in an [[L2LedgerResponse.Applied.ApplyTransaction]]'s effects: append its diffs as an
      * [[EvacuationDiffGroup.Transaction]] group and its payouts, tagging both with the producing
      * `requestId` — the ledger-agnostic withdrawal-effect provenance (all payouts of one
      * transaction share its request).
      */
    def appendTransactionEffects(
        extraDiffs: Vector[EvacuationDiff],
        extraPayouts: Vector[Payout.Obligation],
        requestId: RequestId
    ): L2LedgerInteractionState =
        L2LedgerInteractionState(
          diffs :+ EvacuationDiffGroup.Transaction(requestId, extraDiffs),
          payouts ++ extraPayouts,
          payoutRequestIds ++ Vector.fill(extraPayouts.length)(requestId)
        )

object L2LedgerInteractionState:
    def empty: L2LedgerInteractionState =
        L2LedgerInteractionState(Vector.empty, Vector.empty, Vector.empty)

    /** Protected _specifically_ because we want to prevent arbitrary evolution from the empty
      * state. You _must_ begin with the empty state and evolve it with the `append*` methods
      * (driven by the ledger's [[L2LedgerResponse.Applied]] effects).
      */
    protected[l2] def apply(
        diffs: Vector[EvacuationDiffGroup],
        payouts: Vector[Payout.Obligation],
        payoutRequestIds: Vector[RequestId]
    ) =
        new L2LedgerInteractionState(diffs, payouts, payoutRequestIds)

/** A trait defining an interface to interact with a black-box ledger component (i.e., via the Joint
  * Ledger). The L2Ledger and the state associated with the interactions via the interface are named
  * from the perspective of the _consumer_.
  *
  * Every implementation must be deterministic: consensus feeds each peer's replica the same ordered
  * commands, so a non-deterministic ledger diverges across peers and breaks consensus.
  *
  * Each command method returns a **total** [[L2LedgerResponse]] — its own
  * [[L2LedgerResponse.Applied]] descendant, an optional [[L2LedgerResponse.Rejected]] descendant
  * (user requests only), and the shared [[L2LedgerResponse.UnrecoverableError]] branch (see the
  * per-command `*Response` unions). An outcome (applied / rejected / unrecoverable) is always a
  * response branch, never a raised exception. JointLedger interprets the branch — folding an
  * `Applied` outcome's payload into an [[L2LedgerInteractionState]], invalidating the request on a
  * user-command `Rejected`, and fail-stopping on an `UnrecoverableError` (a desync, a freeze, an
  * unknown deposit compartment, or another internal ledger error). (A `RemoteL2Ledger` may still
  * raise on a broken *transport* — an undecodable frame or a command-number mismatch — which is a
  * protocol violation, not one of the verdicts.)
  *
  * NOTE:
  *   - The constructor of [[L2LedgerInteractionState]] is private. The only way to construct a new
  *     state is via [[L2LedgerInteractionState.empty]], evolved with its `append*` methods.
  *   - Implementors of this trait only need to define the actual methods of sending the requests.
  *
  * @tparam F
  *   A monad in which the "transport" runs. This will be IO for most implementations (for network
  *   or unix socket access, etc), but can also be something like [[State]] for pure implementations
  */
trait L2Ledger[F[_]] {
    implicit def monadF: Monad[F]

    /** See:
      * https://gummiwormlabs.github.io/gummiworm-writing-room/gummiworm-poc/sugar-rush-overview/ledger-events#deposit-events
      * @return
      *   [[L2LedgerResponse.Applied.RegisterDeposit]] (no effects) on success, or a
      *   [[L2LedgerResponse.Rejected]] the caller invalidates the request on.
      */
    def registerDeposit(
        commandNumber: L2CommandNumber,
        req: L2LedgerCommand.RegisterDeposit
    ): F[RegisterDepositResponse]

    /** See:
      * https://gummiwormlabs.github.io/gummiworm-writing-room/gummiworm-poc/sugar-rush-overview/ledger-events#deposit-events
      *
      * A deposit decision is not a user request, so a failure is not an ordinary verdict: deposits
      * are validated at registration, so a decision should never fail on deposit *validity* — only
      * on a coordination bug (a decision for a deposit compartment the ledger never registered, or
      * an internal merge error). Such a failure answers an [[L2LedgerResponse.UnrecoverableError]]
      * **and freezes the ledger**: every subsequent command then answers
      * [[L2LedgerResponse.UnrecoverableError.LedgerFreeze]] until `restoreTo` rewinds past the
      * freeze. JointLedger panics on the error. See `docs/spec/l2-ledger-command-coordination.md`.
      *
      * @return
      *   [[L2LedgerResponse.Applied.ApplyDepositDecisions]] (the evacuation diffs the absorbed
      *   deposits produce), or an [[L2LedgerResponse.UnrecoverableError]] the caller panics on.
      */
    def applyDepositDecisions(
        commandNumber: L2CommandNumber,
        req: L2LedgerCommand.ApplyDepositDecisions
    ): F[ApplyDepositDecisionsResponse]

    /** See:
      * https://gummiwormlabs.github.io/gummiworm-writing-room/gummiworm-poc/sugar-rush-overview/ledger-events#l2-events
      * @return
      *   [[L2LedgerResponse.Applied.ApplyTransaction]] (the evacuation diffs to apply to the
      *   JointLedger's evacuation map plus the payout obligations), or a
      *   [[L2LedgerResponse.Rejected]] the caller invalidates the request on.
      */
    def applyTransaction(
        commandNumber: L2CommandNumber,
        req: L2LedgerCommand.ApplyTransaction
    ): F[ApplyTransactionResponse]

    /** Reconstruct the committed L2 state as of `commandNumber`, from the ledger's own durable
      * record (`(initial state, commandNumber)`; see
      * `docs/spec/l2-ledger-command-coordination.md`). After this the ledger is positioned at
      * `commandNumber`. Used only on crash-recovery boot. JointLedger owns and persists the
      * authoritative command number, so the ledger exposes no read-back query. Implementations that
      * do not persist (e.g. a remote black box that owns its own recovery) may make this a no-op.
      *
      * Unlike the command path, this stays an [[EitherT]]: it is a boot-time reconstruction, not a
      * numbered command, so its failure is a [[RestoreError]] rather than one of the verdicts.
      *
      * Returns the [[EvacuationMapHash]] of the ledger's evacuation map at `commandNumber`, so the
      * caller can check that both sides hold the same map. At a cold start (`commandNumber` zero)
      * that is the check that the head config's `initialEvacuationMap` is the one this ledger
      * actually starts from — see [[RestoreError.EvacuationMapMismatch]].
      */
    def restoreTo(commandNumber: L2CommandNumber): EitherT[F, RestoreError, L2Ledger.Restored]
}

object L2Ledger {

    /** What a ledger reports about itself at a [[L2Ledger.restoreTo]] anchor.
      *
      * The two digests answer different questions, which is why both are here. `evacuationMapHash`
      * moves with every applied command, so at a warm anchor it says only that both sides hold the
      * same *state*. `l2ParamsHash` never moves, so it is what keeps asking whether this is still
      * the right *ledger*. See `design/head-params-hash.md`.
      *
      * @param l2ParamsHash
      *   `None` from a remote ledger that does not report it yet. Transitional: the head then
      *   cannot check which ledger it is driving, so it warns rather than failing. Remove the
      *   `Option` once the remote side ships the field.
      */
    final case class Restored(
        evacuationMapHash: EvacuationMapHash,
        l2ParamsHash: Option[Hash32]
    )
}
