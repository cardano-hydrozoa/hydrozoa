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
  *   - [[CommonChildEvent]] — every-cell core children (BW/JL/FCA/CL/SC/SCA + peer liaison)
  *   - [[HeadOnlyChildEvent]] — RequestSequencer, head-mesh transport, NodeWsServer, hub transports
  *   - [[MultisigOnlyChildEvent]] — fast/slow rate limiters
  */
type HeadMultisigRegimeManagerEvent =
    LifecycleEvent | CommonChildEvent | HeadOnlyChildEvent | MultisigOnlyChildEvent

/** Per-producer tracers shared by every regime+role — what
  * [[MultisigRegimeManagerBase .spawnCoreActors]] needs. Subclasses' cell-specific tracer carriers
  * (HMRM's [[MrmTracers]], CMRM's [[CoilMrmTracers]]) implement this so the base trait sees a
  * uniform surface.
  */
trait HasCoreTracers:
    def blockWeaver: ContraTracer[IO, BlockWeaverEvent]
    def cardanoLiaison: ContraTracer[IO, CardanoLiaisonEvent]
    def fastConsensusActor: ContraTracer[IO, FastConsensusActorEvent]
    def jointLedger: ContraTracer[IO, JointLedgerEvent]
    def stackComposer: ContraTracer[IO, StackComposerEvent]
    def slowConsensusActor: ContraTracer[IO, SlowConsensusActorEvent]
    def peerLiaison: PeerId => ContraTracer[IO, PeerLiaisonEvent]

/** Per-producer projections for the (head, multisig) cell. Holds the core set ([[HasCoreTracers]])
  * plus the head-only + multisig-only producer channels.
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
) extends HasCoreTracers

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
          eventSequencer = tracer.contramap(HeadOnlyChildEvent.EventSequencer.apply),
          blockWeaverLimiter = tracer.contramap(MultisigOnlyChildEvent.BlockWeaverLimiter.apply),
          stackComposerLimiter =
              tracer.contramap(MultisigOnlyChildEvent.StackComposerLimiter.apply),
          coilAckSequencer = tracer.contramap(HeadOnlyChildEvent.CoilAckSequencer.apply),
          peerTransport = tracer.contramap(HeadOnlyChildEvent.PeerTransport.apply),
          hubWsTransport = tracer.contramap(HeadOnlyChildEvent.HubWsTransport.apply),
          nodeWsServer = tracer.contramap(HeadOnlyChildEvent.NodeWsServer.apply),
          peerLiaison = pid => tracer.contramap(CommonChildEvent.PeerLiaison(pid, _)),
        )
