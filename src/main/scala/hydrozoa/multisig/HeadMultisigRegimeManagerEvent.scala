package hydrozoa.multisig

import cats.effect.IO
import cats.syntax.contravariant.*
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.liaison.PeerLiaisonEvent
import hydrozoa.multisig.consensus.limiter.LimiterEvent
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.consensus.transport.{HubWsTransportEvent, NodeWsServerEvent, PeerTransportEvent}
import hydrozoa.multisig.consensus.{BlockWeaverEvent, CardanoLiaisonEvent, CoilAckSequencerEvent, EventSequencerEvent, FastConsensusActorEvent, SlowConsensusActorEvent, StackComposerEvent}
import hydrozoa.multisig.ledger.joint.JointLedgerEvent

/** The (head, multisig) cell of the regime-event grid. A union over the category traits from
  * [[RegimeManagerEvent]] that apply to a head peer running the multisig regime:
  *
  *   - [[LifecycleEvent]] — bring-up + supervision
  *   - [[CommonChildEvent]] — every-cell core children (BW/JL/FCA/CL/SC/SCA/ES + peer liaison)
  *   - [[HeadOnlyChildEvent]] — head-mesh transport, NodeWsServer, hub transports
  *   - [[MultisigOnlyChildEvent]] — fast/slow rate limiters
  *
  * One `ContraTracer[IO, HeadMultisigRegimeManagerEvent]` at the MRM level is `contramap`-ped down
  * to per-actor tracers (`JL`, `FCA`, `CL`, `SC`, `SCA`) inside [[HeadMultisigRegimeManager]]. The
  * wiring layer (`Main` / harness) only has to compose **one** tracer for the whole regime.
  */
type HeadMultisigRegimeManagerEvent =
    LifecycleEvent | CommonChildEvent | HeadOnlyChildEvent | MultisigOnlyChildEvent

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

    /** Derive every per-producer channel from one root MRM-level tracer via `contramap`. The
      * routing is the only piece of wiring that has to know the [[RegimeManagerEvent]] category
      * constructors — every consumer downstream sees the narrow event type.
      */
    def fromRoot(tracer: ContraTracer[IO, HeadMultisigRegimeManagerEvent]): MrmTracers =
        MrmTracers(
          blockWeaver = tracer.contramap(CommonChildEvent.BlockWeaver.apply),
          jointLedger = tracer.contramap(CommonChildEvent.JointLedger.apply),
          fastConsensusActor = tracer.contramap(CommonChildEvent.FastConsensusActor.apply),
          cardanoLiaison = tracer.contramap(CommonChildEvent.CardanoLiaison.apply),
          stackComposer = tracer.contramap(CommonChildEvent.StackComposer.apply),
          slowConsensusActor = tracer.contramap(CommonChildEvent.SlowConsensusActor.apply),
          eventSequencer = tracer.contramap(CommonChildEvent.EventSequencer.apply),
          blockWeaverLimiter = tracer.contramap(MultisigOnlyChildEvent.BlockWeaverLimiter.apply),
          stackComposerLimiter =
              tracer.contramap(MultisigOnlyChildEvent.StackComposerLimiter.apply),
          coilAckSequencer = tracer.contramap(HeadOnlyChildEvent.CoilAckSequencer.apply),
          peerTransport = tracer.contramap(HeadOnlyChildEvent.PeerTransport.apply),
          hubWsTransport = tracer.contramap(HeadOnlyChildEvent.HubWsTransport.apply),
          nodeWsServer = tracer.contramap(HeadOnlyChildEvent.NodeWsServer.apply),
          peerLiaison = pid => tracer.contramap(CommonChildEvent.PeerLiaison(pid, _)),
        )
