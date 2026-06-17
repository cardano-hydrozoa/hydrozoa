package hydrozoa.multisig

import hydrozoa.multisig.HeadMultisigRegimeManager.{Actors, Dependencies}
import hydrozoa.multisig.consensus.liaison.PeerLiaisonEvent
import hydrozoa.multisig.consensus.limiter.LimiterEvent
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.consensus.{BlockWeaverEvent, CardanoLiaisonEvent, EventSequencerEvent, FastConsensusActorEvent, SlowConsensusActorEvent, StackComposerEvent}
import hydrozoa.multisig.ledger.joint.JointLedgerEvent

/** Roll-up of every typed event flowing through the multisig regime. One `ContraTracer[IO,
  * HeadMultisigRegimeManagerEvent]` at the MRM level is `contramap`-ped down to per-actor tracers
  * (`JL`, `FCA`, `CL`, `SC`, `SCA`) inside [[HeadMultisigRegimeManager]]. The wiring layer (`Main`
  * / harness) only has to compose **one** tracer for the whole regime.
  */
sealed trait HeadMultisigRegimeManagerEvent

object HeadMultisigRegimeManagerEvent:
    final case class BW(event: BlockWeaverEvent) extends HeadMultisigRegimeManagerEvent
    final case class JL(event: JointLedgerEvent) extends HeadMultisigRegimeManagerEvent
    final case class FCA(event: FastConsensusActorEvent) extends HeadMultisigRegimeManagerEvent
    final case class CL(event: CardanoLiaisonEvent) extends HeadMultisigRegimeManagerEvent
    final case class SC(event: StackComposerEvent) extends HeadMultisigRegimeManagerEvent
    final case class SCA(event: SlowConsensusActorEvent) extends HeadMultisigRegimeManagerEvent
    final case class ES(event: EventSequencerEvent) extends HeadMultisigRegimeManagerEvent

    /** `remotePeerId` identifies which remote peer this liaison is talking to — a head peer for the
      * mesh and coil-to-hub liaisons, a coil peer for the hub-to-coil liaisons.
      */
    final case class PL(remotePeerId: PeerId, event: PeerLiaisonEvent)
        extends HeadMultisigRegimeManagerEvent
    final case class BWL(event: LimiterEvent) extends HeadMultisigRegimeManagerEvent
    final case class SCL(event: LimiterEvent) extends HeadMultisigRegimeManagerEvent
    case object StartingActors extends HeadMultisigRegimeManagerEvent
    case object WatchingActors extends HeadMultisigRegimeManagerEvent
    final case class TerminatedActor(actor: Actors) extends HeadMultisigRegimeManagerEvent
    final case class TerminatedDependency(dep: HeadMultisigRegimeManager.Dependencies)
        extends HeadMultisigRegimeManagerEvent
