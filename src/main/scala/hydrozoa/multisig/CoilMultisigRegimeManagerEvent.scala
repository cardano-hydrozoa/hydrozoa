package hydrozoa.multisig

import cats.effect.IO
import cats.syntax.contravariant.*
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.liaison.PeerLiaisonEvent
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.consensus.{BlockWeaverEvent, CardanoLiaisonEvent, FastConsensusActorEvent, SlowConsensusActorEvent, StackComposerEvent}
import hydrozoa.multisig.ledger.joint.JointLedgerEvent
import hydrozoa.rulebased.RuleBasedActorEvent

/** The (coil, multisig) cell of the regime-event grid. A union over the category traits from
  * [[RegimeManagerEvent]] that apply to a coil follower running the multisig regime:
  *
  *   - [[LifecycleEvent]] — bring-up + supervision
  *   - [[CommonChildEvent]] — every-cell core children (BW/JL/FCA/CL/SC/SCA + peer liaison)
  *   - [[CoilOnlyChildEvent]] — `CoilPeerWsTransport`
  *   - [[MultisigOnlyChildEvent]] — limiters (today the coil never spawns these; included so the
  *     cell remains within the multisig regime — adding them is a single edit if needed)
  *
  * Excludes [[HeadOnlyChildEvent]] (no RequestSequencer, no head-mesh PeerTransport, no
  * NodeWsServer, no HubWsTransport, no CoilAckSequencer).
  */
type CoilMultisigRegimeManagerEvent =
    LifecycleEvent | CommonChildEvent | CoilOnlyChildEvent | MultisigOnlyChildEvent

/** The full coil-side regime-event surface: the (coil, multisig) cell plus the rule-based children
  * the coil may spawn after the multisig→rule-based handoff (see
  * [[CoilMultisigRegimeManager.onHandoffToRuleBased]]). Mirrors [[HeadRegimeManagerEvent]] on the
  * head side.
  */
type CoilRegimeManagerEvent = CoilMultisigRegimeManagerEvent | RuleBasedOnlyChildEvent

/** Per-producer projections for the (coil, multisig) cell. Today the coil spawns the core set plus,
  * post-handoff, a [[hydrozoa.rulebased.RuleBasedActor]] via
  * [[hydrozoa.rulebased.RuleBasedRegimeManager]]. `CoilPeerWsTransport`'s tracer is allocated
  * outside the regime manager (in the harness / Main), so no field for it lives here yet.
  */
final case class CoilMrmTracers(
    blockWeaver: ContraTracer[IO, BlockWeaverEvent],
    cardanoLiaison: ContraTracer[IO, CardanoLiaisonEvent],
    fastConsensusActor: ContraTracer[IO, FastConsensusActorEvent],
    jointLedger: ContraTracer[IO, JointLedgerEvent],
    stackComposer: ContraTracer[IO, StackComposerEvent],
    slowConsensusActor: ContraTracer[IO, SlowConsensusActorEvent],
    peerLiaison: PeerId => ContraTracer[IO, PeerLiaisonEvent],
    ruleBasedActor: ContraTracer[IO, RuleBasedActorEvent],
) extends HasCoreTracers

object CoilMrmTracers:
    def fromRoot(tracer: ContraTracer[IO, CoilRegimeManagerEvent]): CoilMrmTracers =
        CoilMrmTracers(
          blockWeaver = tracer.contramap(CommonChildEvent.BlockWeaver.apply),
          cardanoLiaison = tracer.contramap(CommonChildEvent.CardanoLiaison.apply),
          fastConsensusActor = tracer.contramap(CommonChildEvent.FastConsensusActor.apply),
          jointLedger = tracer.contramap(CommonChildEvent.JointLedger.apply),
          stackComposer = tracer.contramap(CommonChildEvent.StackComposer.apply),
          slowConsensusActor = tracer.contramap(CommonChildEvent.SlowConsensusActor.apply),
          peerLiaison = pid => tracer.contramap(CommonChildEvent.PeerLiaison(pid, _)),
          ruleBasedActor = tracer.contramap(RuleBasedOnlyChildEvent.RuleBasedActor.apply),
        )
