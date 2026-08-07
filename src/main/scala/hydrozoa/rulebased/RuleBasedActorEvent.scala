package hydrozoa.rulebased

import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx
import scalus.cardano.address.ShelleyAddress

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
        final case class Found(value: String) extends RuleBasedActorEvent
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

        /** Residual set still contains an AwaitingVote ballot and the voting deadline has not
          * elapsed; tally is deferred until on-chain time crosses the deadline.
          */
        case object WaitingForVotesBeforeDeadline extends RuleBasedActorEvent

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

    object Tx:
        final case class Building(family: String) extends RuleBasedActorEvent
        final case class Submitting(tx: EnrichedTx[?]) extends RuleBasedActorEvent
        final case class SubmitSuccess(tx: EnrichedTx[?]) extends RuleBasedActorEvent
        case object Tallying extends RuleBasedActorEvent

    object Evacuation:
        case object NoMore extends RuleBasedActorEvent
        final case class PayoutsLeft(n: Int) extends RuleBasedActorEvent

        /** DIAGNOSTIC: the candidate evacuation-map commitments loaded at evacuation time, and the
          * latest hard-confirmed stack they were derived from — logged to compare against the
          * commitment the treasury actually resolved to (see [[ResolvedKzg]]).
          */
        final case class CandidateMaps(latestHardConfirmed: String, candidateKzgs: List[String])
            extends RuleBasedActorEvent

        /** DIAGNOSTIC: the commitment the resolution wrote into the treasury, looked up against the
          * candidate set. A miss is the `UnknownResolvedKzg` crash.
          */
        final case class ResolvedKzg(kzg: String) extends RuleBasedActorEvent

        /** DIAGNOSTIC: where the evacuation candidate walk anchored — the Major stack it landed on
          * (vs. the latest hard-confirmed stack), the default-vote map's source block + commitment,
          * and every SEC commitment it collected (as `block=<n> kzg=<hex>`). Shows whether a
          * resolved commitment was excluded because a later minor-only stack was skipped or because
          * the default map is mis-keyed.
          */
        final case class EvacuationAnchor(
            anchorStack: String,
            defaultMapBlock: String,
            defaultKzg: String,
            secs: List[String]
        ) extends RuleBasedActorEvent
