package hydrozoa.multisig

import hydrozoa.multisig.HeadMultisigRegimeManager.{Actors, Dependencies}
import hydrozoa.multisig.consensus.liaison.PeerLiaisonEvent
import hydrozoa.multisig.consensus.limiter.LimiterEvent
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.consensus.transport.{CoilPeerWsTransportEvent, HubWsTransportEvent, NodeWsServerEvent, PeerTransportEvent}
import hydrozoa.multisig.consensus.{BlockWeaverEvent, CardanoLiaisonEvent, CoilAckSequencerEvent, EventSequencerEvent, FastConsensusActorEvent, SlowConsensusActorEvent, StackComposerEvent}
import hydrozoa.multisig.ledger.joint.JointLedgerEvent

/** Top of a 2-dim event hierarchy for the regime managers, sliced along the two axes the regime
  * managers themselves are sliced on:
  *
  *   - **role**: head or coil — head peers spawn a request sequencer and a head-mesh liaison set;
  *     coil peers spawn one uplink liaison.
  *   - **regime**: multisig (today) or rule-based (future) — multisig spawns the fast + slow
  *     consensus stack with rate limiters; rule-based will spawn its own dispute/evacuation
  *     surface.
  *
  * Each concrete cell (HMRM today, CMRM today, head/coil rule-based later) is a union-type alias
  * over the category traits below, picking the subset that applies. New cells = one type alias.
  *
  * See [[HeadMultisigRegimeManagerEvent]] for the legacy single-cell roll-up that still feeds the
  * MRM tracers today; this file lays the categories down so the cells can be rewired piece by
  * piece.
  */
sealed trait RegimeManagerEvent

/** Bring-up + supervision events. Shared by every cell. */
sealed trait LifecycleEvent extends RegimeManagerEvent
object LifecycleEvent:
    case object StartingActors extends LifecycleEvent
    case object WatchingActors extends LifecycleEvent
    final case class TerminatedActor(actor: Actors) extends LifecycleEvent
    final case class TerminatedDependency(dep: Dependencies) extends LifecycleEvent

/** Child actors every regime+role spawns — the "core" set in
  * [[MultisigRegimeManagerBase.spawnCoreActors]] plus the peer liaison (which both head-mesh and
  * coil-uplink reuse, tagged by `remotePeerId`).
  */
sealed trait CommonChildEvent extends RegimeManagerEvent
object CommonChildEvent:
    final case class BlockWeaver(event: BlockWeaverEvent) extends CommonChildEvent
    final case class CardanoLiaison(event: CardanoLiaisonEvent) extends CommonChildEvent
    final case class JointLedger(event: JointLedgerEvent) extends CommonChildEvent
    final case class FastConsensusActor(event: FastConsensusActorEvent) extends CommonChildEvent
    final case class StackComposer(event: StackComposerEvent) extends CommonChildEvent
    final case class SlowConsensusActor(event: SlowConsensusActorEvent) extends CommonChildEvent
    final case class EventSequencer(event: EventSequencerEvent) extends CommonChildEvent

    /** Peer-to-peer liaison; tagged by remote peer (head or coil). Same case carries both head-mesh
      * liaisons on a head peer and the single coil-to-hub uplink on a coil.
      */
    final case class PeerLiaison(remotePeerId: PeerId, event: PeerLiaisonEvent)
        extends CommonChildEvent

/** Children only head peers spawn (in any regime). */
sealed trait HeadOnlyChildEvent extends RegimeManagerEvent
object HeadOnlyChildEvent:
    final case class PeerTransport(event: PeerTransportEvent) extends HeadOnlyChildEvent
    final case class NodeWsServer(event: NodeWsServerEvent) extends HeadOnlyChildEvent

    /** Only present on a hub head peer. */
    final case class HubWsTransport(event: HubWsTransportEvent) extends HeadOnlyChildEvent

    /** Only present on a hub head peer. */
    final case class CoilAckSequencer(event: CoilAckSequencerEvent) extends HeadOnlyChildEvent

/** Children only coil peers spawn (in any regime). */
sealed trait CoilOnlyChildEvent extends RegimeManagerEvent
object CoilOnlyChildEvent:
    final case class CoilPeerWsTransport(event: CoilPeerWsTransportEvent) extends CoilOnlyChildEvent

/** Children only the multisig regime spawns (in any role). */
sealed trait MultisigOnlyChildEvent extends RegimeManagerEvent
object MultisigOnlyChildEvent:
    final case class BlockWeaverLimiter(event: LimiterEvent) extends MultisigOnlyChildEvent
    final case class StackComposerLimiter(event: LimiterEvent) extends MultisigOnlyChildEvent

/** Children only the rule-based regime spawns (in any role). Populated when PR 2 lands. */
sealed trait RuleBasedOnlyChildEvent extends RegimeManagerEvent
object RuleBasedOnlyChildEvent
