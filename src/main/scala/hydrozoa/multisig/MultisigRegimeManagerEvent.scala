package hydrozoa.multisig

import hydrozoa.multisig.MultisigRegimeManager.{Actors, Dependencies}
import hydrozoa.multisig.consensus.{CardanoLiaisonEvent, FastConsensusActorEvent, SlowConsensusActorEvent, StackComposerEvent}
import hydrozoa.multisig.ledger.joint.JointLedgerEvent

/** Roll-up of every typed event flowing through the multisig regime. One `ContraTracer[IO,
  * MultisigRegimeManagerEvent]` at the MRM level is `contramap`-ped down to per-actor tracers
  * (`JL`, `FCA`, `CL`, `SC`, `SCA`) inside [[MultisigRegimeManager]]. The wiring layer (`Main` /
  * harness) only has to compose **one** tracer for the whole regime.
  */
sealed trait MultisigRegimeManagerEvent

object MultisigRegimeManagerEvent:
    final case class JL(event: JointLedgerEvent) extends MultisigRegimeManagerEvent
    final case class FCA(event: FastConsensusActorEvent) extends MultisigRegimeManagerEvent
    final case class CL(event: CardanoLiaisonEvent) extends MultisigRegimeManagerEvent
    final case class SC(event: StackComposerEvent) extends MultisigRegimeManagerEvent
    final case class SCA(event: SlowConsensusActorEvent) extends MultisigRegimeManagerEvent
    case object StartingActors extends MultisigRegimeManagerEvent
    case object WatchingActors extends MultisigRegimeManagerEvent
    final case class TerminatedActor(actor: Actors) extends MultisigRegimeManagerEvent
    final case class TerminatedDependency(dep: MultisigRegimeManager.Dependencies)
        extends MultisigRegimeManagerEvent
