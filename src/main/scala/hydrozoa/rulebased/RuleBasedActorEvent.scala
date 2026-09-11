package hydrozoa.rulebased

import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{TransactionHash, Value}

sealed trait RuleBasedActorEvent

object RuleBasedActorEvent:

    object Backend:
        final case class ErrorDisputeUtxos(e: CardanoBackend.Error) extends RuleBasedActorEvent
        final case class ErrorTreasuryUtxos(e: CardanoBackend.Error) extends RuleBasedActorEvent
        final case class ErrorRegimeUtxos(e: CardanoBackend.Error) extends RuleBasedActorEvent
        final case class ErrorPeerUtxos(e: CardanoBackend.Error) extends RuleBasedActorEvent
        final case class ErrorFeeUtxos(e: CardanoBackend.Error) extends RuleBasedActorEvent
        final case class ErrorContinuingTxs(e: CardanoBackend.Error) extends RuleBasedActorEvent
        final case class ErrorSubmittingTx(e: CardanoBackend.Error) extends RuleBasedActorEvent

    object Treasury:
        case object Querying extends RuleBasedActorEvent
        final case class Found(value: Value) extends RuleBasedActorEvent
        case object NotFound extends RuleBasedActorEvent
        case object Parsing extends RuleBasedActorEvent
        case object ParsedUnresolved extends RuleBasedActorEvent
        case object ParsedResolved extends RuleBasedActorEvent

    object Regime:
        case object Querying extends RuleBasedActorEvent
        case object Found extends RuleBasedActorEvent
        case object NotFound extends RuleBasedActorEvent

    object Collateral:
        final case class Querying(address: ShelleyAddress) extends RuleBasedActorEvent
        case object Found extends RuleBasedActorEvent
        final case class NotFound(address: ShelleyAddress) extends RuleBasedActorEvent

    object Fee:
        final case class Querying(address: ShelleyAddress) extends RuleBasedActorEvent

    object Dispute:
        case object Querying extends RuleBasedActorEvent
        case object Parsing extends RuleBasedActorEvent
        case object ParsingCastVote extends RuleBasedActorEvent
        case object ParsingTally extends RuleBasedActorEvent
        case object ParsingResolve extends RuleBasedActorEvent
        case object ParsingEmptyVotes extends RuleBasedActorEvent

        /** The voting deadline has not elapsed; tally is deferred until on-chain time crosses it. A
          * public Voted box can be ratcheted by anyone until the deadline, so no ballot is final
          * and the TallyTx's validity range starts at the deadline.
          */
        case object WaitingForVotingDeadline extends RuleBasedActorEvent

        /** Voting deadline has elapsed while the peer's own path (cast a vote for a head peer;
          * ratchet an open box for a coil peer) is still applicable. On-chain the corresponding tx
          * would fail the validity range, so we skip it and dispatch to the residual tally/resolve
          * path immediately.
          */
        case object VotingDeadlineElapsed extends RuleBasedActorEvent

        /** Coil-peer classifier outcomes for the coil ratchet path (see
          * [[RuleBasedActor.Dispute.handleCoil]]).
          */
        object Coil:
            case object ParsingRatchet extends RuleBasedActorEvent
            case object AlreadyAtTarget extends RuleBasedActorEvent
            case object NoRatchetTarget extends RuleBasedActorEvent

    object Tick:
        /** The tick's `EitherT` ended in a recoverable error that the actor swallows and retries on
          * the next tick. Emitted so that "recoverably fails and retries" is never fully silent —
          * kept at `debug` since a healthy backend hits these only transiently; the specific
          * evacuation stall has its own `info` event ([[Evacuation.ContinuingTreasuryTxs]]).
          */
        final case class RecoverableRetry(reason: String) extends RuleBasedActorEvent

    object Tx:
        final case class Building(family: String) extends RuleBasedActorEvent
        final case class Submitting(tx: EnrichedTx[?]) extends RuleBasedActorEvent
        final case class SubmitSuccess(tx: EnrichedTx[?]) extends RuleBasedActorEvent
        case object Tallying extends RuleBasedActorEvent

    object Evacuation:
        case object NoMore extends RuleBasedActorEvent
        final case class PayoutsLeft(n: Int) extends RuleBasedActorEvent

        /** How many continuing treasury txs the backend surfaced after the fallback anchor
          * (fallback → resolution → withdrawals). Zero means the resolution tx hasn't surfaced on
          * the backend yet (eventual-consistency lag or a rollback), so `loadEvacuationState`
          * recoverably retries from `NoTreasuryFound` — traced (unlike that empty-list path itself)
          * so an evacuation stalled waiting for the chain is visible, and so a growing count shows
          * the chain converging.
          */
        final case class ContinuingTreasuryTxs(count: Int) extends RuleBasedActorEvent

        /** DIAGNOSTIC: the candidate evacuation-map commitments loaded at evacuation time, and the
          * latest hard-confirmed stack they were derived from — logged to compare against the
          * commitment the treasury actually resolved to (see [[ResolvedKzg]]).
          */
        final case class CandidateMaps(
            latestHardConfirmed: String,
            candidateKzgs: Set[KzgCommitment]
        ) extends RuleBasedActorEvent

        /** DIAGNOSTIC: the commitment the resolution wrote into the treasury, looked up against the
          * candidate set. A miss is the `UnknownResolvedKzg` crash.
          */
        final case class ResolvedKzg(kzg: KzgCommitment) extends RuleBasedActorEvent

        /** DIAGNOSTIC: the provenance of the candidate evacuation-map set — the default-vote map's
          * source block + commitment, and every votable SEC commitment collected (as
          * `block=<n> kzg=<hex>`). Shows whether a resolved commitment was excluded because the
          * default map is mis-keyed or a votable SEC was missed. The final deduped kzg set is
          * [[CandidateMaps]]; where the fallback chain anchors is [[EvacuationAnchor]].
          */
        final case class CandidateMapSources(
            defaultMapBlock: String,
            defaultKzg: String,
            secs: List[String]
        ) extends RuleBasedActorEvent

        /** DIAGNOSTIC: where the fallback-anchor walk landed — the hard-confirmed stack carrying
          * the fallback tx that anchors the continuing-treasury-tx query, and that tx's id. A
          * minor-only stack is walked past; the stack shown is the one that actually supplied the
          * anchor.
          */
        final case class EvacuationAnchor(anchorStack: String, fallbackTxId: TransactionHash)
            extends RuleBasedActorEvent
