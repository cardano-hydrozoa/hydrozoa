package hydrozoa.multisig

import hydrozoa.multisig.MultisigRegimeManager.{Actors, Dependencies}
import hydrozoa.multisig.consensus.liaison.PeerLiaisonEvent
import hydrozoa.multisig.consensus.limiter.LimiterEvent
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.consensus.{BlockWeaverEvent, CardanoLiaisonEvent, EventSequencerEvent, FastConsensusActorEvent, SlowConsensusActorEvent, StackComposerEvent}
import hydrozoa.multisig.ledger.joint.JointLedgerEvent

/** Roll-up of every typed event flowing through the multisig regime. One `ContraTracer[IO,
  * MultisigRegimeManagerEvent]` at the MRM level is `contramap`-ped down to per-actor tracers
  * (`JL`, `FCA`, `CL`, `SC`, `SCA`) inside [[MultisigRegimeManager]]. The wiring layer (`Main` /
  * harness) only has to compose **one** tracer for the whole regime.
  */
sealed trait MultisigRegimeManagerEvent

object MultisigRegimeManagerEvent:
    final case class BW(event: BlockWeaverEvent) extends MultisigRegimeManagerEvent
    final case class JL(event: JointLedgerEvent) extends MultisigRegimeManagerEvent
    final case class FCA(event: FastConsensusActorEvent) extends MultisigRegimeManagerEvent
    final case class CL(event: CardanoLiaisonEvent) extends MultisigRegimeManagerEvent
    final case class SC(event: StackComposerEvent) extends MultisigRegimeManagerEvent
    final case class SCA(event: SlowConsensusActorEvent) extends MultisigRegimeManagerEvent
    final case class ES(event: EventSequencerEvent) extends MultisigRegimeManagerEvent

    /** `remotePeerId` identifies which remote peer this liaison is talking to — a head peer for the
      * mesh and coil-to-hub liaisons, a coil peer for the hub-to-coil liaisons.
      */
    final case class PL(remotePeerId: PeerId, event: PeerLiaisonEvent)
        extends MultisigRegimeManagerEvent
    final case class BWL(event: LimiterEvent) extends MultisigRegimeManagerEvent
    final case class SCL(event: LimiterEvent) extends MultisigRegimeManagerEvent
    case object StartingActors extends MultisigRegimeManagerEvent
    case object WatchingActors extends MultisigRegimeManagerEvent
    final case class TerminatedActor(actor: Actors) extends MultisigRegimeManagerEvent
    final case class TerminatedDependency(dep: MultisigRegimeManager.Dependencies)
        extends MultisigRegimeManagerEvent
