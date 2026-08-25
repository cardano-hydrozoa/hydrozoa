package hydrozoa.multisig

import cats.*
import cats.effect.{Deferred, IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.NoSendActorRef
import com.suprnation.actor.SupervisorStrategy.{Directive, Escalate}
import com.suprnation.actor.{ActorContext, OneForOneStrategy, SupervisionStrategy}
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.HeadMultisigRegimeManager.*
import hydrozoa.multisig.MultisigRegimeManagerBase.CoreActors
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.*
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.l2.L2Ledger
import hydrozoa.multisig.metrics.PeerMetrics
import hydrozoa.multisig.persistence.Persistence
import scala.concurrent.duration.DurationInt

/** Shared scaffolding for [[HeadMultisigRegimeManager]] and [[CoilMultisigRegimeManager]]: the
  * supervisor strategy, the boot pre-start dispatch + `Connections` barrier, the per-producer
  * tracer specialization for the actors that exist on both head and coil peers, and a
  * [[spawnCoreActors]] helper that allocates that shared set.
  *
  * Subclasses implement [[preStartLocal]] to fill in the role-specific pieces — head-only:
  * `RequestSequencer`, limiters, the head mesh, and the optional hub-side coil relay; coil-only:
  * the single hub uplink liaison. Each subclass supplies [[tracer]] as an `override val`
  * constructor parameter (Scala 3 disallows traits passing args to a parameterized parent trait, so
  * the tracer is wired in via abstract-member override instead).
  */
trait MultisigRegimeManagerBase[E >: LifecycleEvent <: RegimeManagerEvent]
    extends Actor[IO, Request] {

    /** Regime-wide tracer, supplied by the subclass (typically as an `override val` constructor
      * parameter). The cell type `E` constrains which categories the subclass may emit;
      * `E >: LifecycleEvent` lets the base trait emit lifecycle events here without further
      * narrowing.
      */
    protected def tracer: ContraTracer[IO, E]

    /** Per-producer projections of [[tracer]] for the actors the base trait spawns. Subclasses
      * supply their cell-specific carrier (e.g. [[MrmTracers]] for head, [[CoilMrmTracers]] for
      * coil); both implement [[HasCoreTracers]] so [[spawnCoreActors]] sees a uniform surface.
      */
    protected def tracers: HasCoreTracers

    /** The peer metrics registry, supplied by the subclass and threaded into the instrumented
      * actors (see `docs/spec/peer-stats-endpoint.md`).
      */
    protected def metrics: PeerMetrics

    /** Completed by the subclass's [[preStartLocal]] once every actor is spawned and the
      * `Connections` slots are populated.
      */
    val connectionsDeferred: Deferred[IO, Connections] = Deferred.unsafe[IO, Connections]

    /** Node lifecycle status backing the user-facing server's `/ready` endpoint. Advanced
      * monotonically (via [[NodeStatus.advanceTo]]) by [[CardanoLiaison]] as the L1 target state
      * changes and by this manager on [[HandoffToRuleBased]]; never read here.
      */
    val nodeStatus: Ref[IO, NodeStatus] = Ref.unsafe[IO, NodeStatus](NodeStatus.Initializing)

    /** Every failure escalates, and the decider is **total**.
      *
      * Totality is what the `PartialFunction.fromFunction` wrapper buys: `isDefinedAt` is
      * unconditionally true, so the supervisor can never see this decider as undefined. A failure
      * outside a decider's domain is not a handled failure — it surfaces as `emergency stop:
      * exception in failure handling for …`, which discards the original error and leaves whatever
      * waits on this subtree waiting forever: a silent hang rather than a loud failure. Stopping at
      * `case _: Exception` reads as exhaustive and is not, since `Throwable`'s other child is
      * `Error`, and Scala applies no checked-exception pressure to make a bare `Throwable`
      * conspicuous.
      */
    override def supervisorStrategy: SupervisionStrategy[IO] =
        new OneForOneStrategy[IO](maxNrOfRetries = 3, withinTimeRange = 1.minute)(
          PartialFunction.fromFunction {
              case _: IllegalArgumentException =>
                  Escalate // Normally `Stop` but we can't handle stopped actors yet
              case _: RuntimeException =>
                  Escalate // Normally `Restart` but our actors can't do that yet
              case _: Exception => Escalate
              // `Error`, and anything extending `Throwable` directly. Keeping this arm last leaves
              // the intent of the arms above legible for when they can take their real directives.
              case _ => Escalate
          }
        ) {
            override def logFailure(
                context: ActorContext[IO, ?, ?],
                child: NoSendActorRef[IO],
                cause: Option[Throwable],
                decision: Directive
            ): IO[Unit] =
                decision match
                    case Escalate =>
                        cause.traverse_(c =>
                            tracer.traceWith(LifecycleEvent.SupervisedFailureEscalated(c))
                        )
                    case _ => super.logFailure(context, child, cause, decision)
        }

    override def preStart: IO[Unit] = context.self ! PreStart

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: Request): IO[Unit] = req match {
        case PreStart => preStartLocal
        case TerminatedChild(childType, _) =>
            tracer.traceWith(LifecycleEvent.TerminatedActor(childType))
        case TerminatedDependency(dependencyType, _) =>
            tracer.traceWith(LifecycleEvent.TerminatedDependency(dependencyType))
        case HandoffToRuleBased =>
            nodeStatus.update(_.advanceTo(NodeStatus.HandedOffToRuleBased)) *>
                onHandoffToRuleBased
        // TODO: Implement a way to receive a remote comm actor and connect it to its corresponding local comm actor
    }

    /** Role-specific boot: spawn the role's full actor set, populate `Connections`, replay state,
      * then complete the `pendingConnections` + [[connectionsDeferred]] barriers.
      */
    protected def preStartLocal: IO[Unit]

    /** React to [[HandoffToRuleBased]] by stopping the multisig actors (except the still-live
      * [[CardanoLiaison]]) and spawning the rule-based regime manager. Subclass-supplied per role:
      * HMRM spawns [[hydrozoa.rulebased.RuleBasedRegimeManager]]; CMRM will spawn the coil-side
      * equivalent. Abstract on purpose — a silent default would swallow the handoff.
      */
    protected def onHandoffToRuleBased: IO[Unit]

    /** Fan a list of (actorRef, actor-kind) pairs into per-child death watches that fire
      * `TerminatedChild` back to this manager.
      */
    protected def watchChildren(pairs: (NoSendActorRef[IO], Actors)*): IO[Unit] =
        pairs.toList.traverse_ { (ref, actor) => context.watch(ref, TerminatedChild(actor, ref)) }

    /** Spawn the actors that exist on both head and coil peers: the fast-cycle producers, the
      * Cardano liaison, the joint ledger, and the slow-cycle producers. Limiters, request
      * sequencer, and head-mesh liaisons are head-only and stay in [[HeadMultisigRegimeManager]];
      * the hub uplink stays in [[CoilMultisigRegimeManager]].
      */
    protected def spawnCoreActors(
        config: NodeConfig,
        cardanoBackend: CardanoBackend[IO],
        l2Ledger: L2Ledger[IO],
        persistence: Persistence[IO],
        pendingConnections: Deferred[IO, Connections],
    ): IO[CoreActors] =
        for {
            blockWeaver <- context.actorOf(
              BlockWeaver(config, pendingConnections, tracers.blockWeaver, metrics, persistence)
            )
            cardanoLiaison <- context.actorOf(
              CardanoLiaison(
                config,
                cardanoBackend,
                pendingConnections,
                tracers.cardanoLiaison,
                persistence,
                // In the multisig regime, observing the rule-based treasury on L1 fires the handoff
                // to this manager, which stops the multisig actors and spawns the rule-based regime
                // (`onHandoffToRuleBased`). The rule-based regime's own `CardanoLiaison` supplies a
                // no-op here instead (design `docs/spec/evacuate-command.md`).
                onRuleBasedRegimeObserved = _ => context.self ! HandoffToRuleBased,
                advanceNodeStatus = next => nodeStatus.update(_.advanceTo(next)),
              )
            )
            consensusActor <- context.actorOf(
              FastConsensusActor(
                config,
                pendingConnections,
                tracers.fastConsensusActor,
                persistence,
                metrics
              )
            )
            jointLedger <- context.actorOf(
              JointLedger(
                config,
                pendingConnections,
                l2Ledger,
                tracers.jointLedger,
                persistence,
                metrics
              )
            )
            stackComposer <- context.actorOf(
              StackComposer(config, pendingConnections, tracers.stackComposer, persistence)
            )
            slowConsensusActor <- context.actorOf(
              SlowConsensusActor(
                config,
                pendingConnections,
                tracers.slowConsensusActor,
                persistence,
                metrics
              )
            )
        } yield CoreActors(
          blockWeaver,
          cardanoLiaison,
          consensusActor,
          jointLedger,
          stackComposer,
          slowConsensusActor,
        )
}

object MultisigRegimeManagerBase {

    /** The actors spawned by [[MultisigRegimeManagerBase.spawnCoreActors]] — present on every
      * multisig peer regardless of role.
      */
    final case class CoreActors(
        blockWeaver: BlockWeaver.Handle,
        cardanoLiaison: CardanoLiaison.Handle,
        consensusActor: FastConsensusActor.Handle,
        jointLedger: JointLedger.Handle,
        stackComposer: StackComposer.Handle,
        slowConsensusActor: SlowConsensusActor.Handle,
    )
}
