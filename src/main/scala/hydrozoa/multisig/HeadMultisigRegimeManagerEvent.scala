package hydrozoa.multisig

import cats.effect.IO
import cats.syntax.contravariant.*
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.HeadMultisigRegimeManager.{Actors, Dependencies}
import hydrozoa.multisig.consensus.liaison.PeerLiaisonEvent
import hydrozoa.multisig.consensus.limiter.LimiterEvent
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.consensus.transport.{HubWsTransportEvent, NodeWsServerEvent, PeerTransportEvent}
import hydrozoa.multisig.consensus.{BlockWeaverEvent, CardanoLiaisonEvent, CoilAckSequencerEvent, EventSequencerEvent, FastConsensusActorEvent, SlowConsensusActorEvent, StackComposerEvent}
import hydrozoa.multisig.ledger.joint.JointLedgerEvent

/** Roll-up of every typed event flowing through the multisig regime. One `ContraTracer[IO,
  * HeadMultisigRegimeManagerEvent]` at the MRM level is `contramap`-ped down to per-actor tracers
  * (`JL`, `FCA`, `CL`, `SC`, `SCA`) inside [[HeadMultisigRegimeManager]]. The wiring layer (`Main`
  * / harness) only has to compose **one** tracer for the whole regime.
  */
sealed trait HeadMultisigRegimeManagerEvent

object HeadMultisigRegimeManagerEvent:
    final case class BlockWeaver(event: BlockWeaverEvent) extends HeadMultisigRegimeManagerEvent
    final case class JointLedger(event: JointLedgerEvent) extends HeadMultisigRegimeManagerEvent
    final case class FastConsensusActor(event: FastConsensusActorEvent)
        extends HeadMultisigRegimeManagerEvent
    final case class CardanoLiaison(event: CardanoLiaisonEvent)
        extends HeadMultisigRegimeManagerEvent
    final case class StackComposer(event: StackComposerEvent) extends HeadMultisigRegimeManagerEvent
    final case class SlowConsensusActor(event: SlowConsensusActorEvent)
        extends HeadMultisigRegimeManagerEvent
    final case class EventSequencer(event: EventSequencerEvent)
        extends HeadMultisigRegimeManagerEvent

    /** `remotePeerId` identifies which remote peer this liaison is talking to — a head peer for the
      * mesh and coil-to-hub liaisons, a coil peer for the hub-to-coil liaisons.
      */
    final case class PeerLiaison(remotePeerId: PeerId, event: PeerLiaisonEvent)
        extends HeadMultisigRegimeManagerEvent
    final case class BlockWeaverLimiter(event: LimiterEvent) extends HeadMultisigRegimeManagerEvent
    final case class StackComposerLimiter(event: LimiterEvent)
        extends HeadMultisigRegimeManagerEvent
    final case class PeerTransport(event: PeerTransportEvent) extends HeadMultisigRegimeManagerEvent
    final case class HubWsTransport(event: HubWsTransportEvent)
        extends HeadMultisigRegimeManagerEvent
    final case class NodeWsServer(event: NodeWsServerEvent) extends HeadMultisigRegimeManagerEvent
    final case class CoilAckSequencer(event: CoilAckSequencerEvent)
        extends HeadMultisigRegimeManagerEvent
    case object StartingActors extends HeadMultisigRegimeManagerEvent
    case object WatchingActors extends HeadMultisigRegimeManagerEvent
    final case class TerminatedActor(actor: Actors) extends HeadMultisigRegimeManagerEvent
    final case class TerminatedDependency(dep: HeadMultisigRegimeManager.Dependencies)
        extends HeadMultisigRegimeManagerEvent

/** Per-producer projections of one MRM-level [[ContraTracer]], in carrier form. One contramap per
  * variant of [[HeadMultisigRegimeManagerEvent]] that carries a typed sub-event (`PL` is keyed by
  * its remote-peer identity since each liaison gets its own contextualized sink). Build via
  * [[MrmTracers.fromRoot]]; subclasses of [[MultisigRegimeManagerBase]] hold one of these and pass
  * the per-actor channel into each producer constructor.
  */
final case class MrmTracers(
    blockWeaver: ContraTracer[IO, BlockWeaverEvent],
    jointLedger: ContraTracer[IO, JointLedgerEvent],
    fastConsensusActor: ContraTracer[IO, FastConsensusActorEvent],
    cardanoLiaison: ContraTracer[IO, CardanoLiaisonEvent],
    stackComposer: ContraTracer[IO, StackComposerEvent],
    slowConsensusActor: ContraTracer[IO, SlowConsensusActorEvent],
    eventSequencer: ContraTracer[IO, EventSequencerEvent],
    blockWeaverLimiter: ContraTracer[IO, LimiterEvent],
    stackComposerLimiter: ContraTracer[IO, LimiterEvent],
    coilAckSequencer: ContraTracer[IO, CoilAckSequencerEvent],
    peerTransport: ContraTracer[IO, PeerTransportEvent],
    hubWsTransport: ContraTracer[IO, HubWsTransportEvent],
    nodeWsServer: ContraTracer[IO, NodeWsServerEvent],
    peerLiaison: PeerId => ContraTracer[IO, PeerLiaisonEvent],
)

object MrmTracers:
    import HeadMultisigRegimeManagerEvent as MRMEvent

    /** Derive every per-producer channel from one root MRM-level tracer via `contramap`. The
      * routing is the only piece of wiring that has to know the [[HeadMultisigRegimeManagerEvent]]
      * variant constructors — every consumer downstream sees the narrow event type.
      */
    def fromRoot(tracer: ContraTracer[IO, HeadMultisigRegimeManagerEvent]): MrmTracers =
        MrmTracers(
          blockWeaver = tracer.contramap(MRMEvent.BlockWeaver.apply),
          jointLedger = tracer.contramap(MRMEvent.JointLedger.apply),
          fastConsensusActor = tracer.contramap(MRMEvent.FastConsensusActor.apply),
          cardanoLiaison = tracer.contramap(MRMEvent.CardanoLiaison.apply),
          stackComposer = tracer.contramap(MRMEvent.StackComposer.apply),
          slowConsensusActor = tracer.contramap(MRMEvent.SlowConsensusActor.apply),
          eventSequencer = tracer.contramap(MRMEvent.EventSequencer.apply),
          blockWeaverLimiter = tracer.contramap(MRMEvent.BlockWeaverLimiter.apply),
          stackComposerLimiter = tracer.contramap(MRMEvent.StackComposerLimiter.apply),
          coilAckSequencer = tracer.contramap(MRMEvent.CoilAckSequencer.apply),
          peerTransport = tracer.contramap(MRMEvent.PeerTransport.apply),
          hubWsTransport = tracer.contramap(MRMEvent.HubWsTransport.apply),
          nodeWsServer = tracer.contramap(MRMEvent.NodeWsServer.apply),
          peerLiaison = pid => tracer.contramap(MRMEvent.PeerLiaison(pid, _)),
        )
