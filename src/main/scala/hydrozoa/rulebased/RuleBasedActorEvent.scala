package hydrozoa.rulebased

import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx
import scalus.cardano.address.ShelleyAddress

sealed trait RuleBasedActorEvent

object RuleBasedActorEvent:

    object Backend:
        final case class ErrorDisputeUtxos(e: CardanoBackend.Error) extends RuleBasedActorEvent
        final case class ErrorTreasuryUtxos(e: CardanoBackend.Error) extends RuleBasedActorEvent
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
