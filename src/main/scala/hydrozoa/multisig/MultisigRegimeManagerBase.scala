package hydrozoa.multisig

import cats.*
import cats.effect.{Deferred, IO}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.NoSendActorRef
import com.suprnation.actor.SupervisorStrategy.Escalate
import com.suprnation.actor.{OneForOneStrategy, SupervisionStrategy}
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.HeadMultisigRegimeManager.*
import hydrozoa.multisig.HeadMultisigRegimeManagerEvent as MRMEvent
import hydrozoa.multisig.HeadMultisigRegimeManagerEvent.TerminatedActor
import hydrozoa.multisig.MultisigRegimeManagerBase.CoreActors
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.*
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.l2.L2Ledger
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
trait MultisigRegimeManagerBase extends Actor[IO, Request] {

    /** Regime-wide tracer, supplied by the subclass (typically as an `override val` constructor
      * parameter). Producer-specific channels are derived from this via [[tracers]].
      */
    protected def tracer: ContraTracer[IO, HeadMultisigRegimeManagerEvent]

    /** Per-producer projections of [[tracer]]. `lazy` because `tracer` is abstract — the subclass's
      * `val tracer` may not be initialized yet when the base trait's fields are constructed.
      */
    protected lazy val tracers: MrmTracers = MrmTracers.fromRoot(tracer)

    /** Completed by the subclass's [[preStartLocal]] once every actor is spawned and the
      * `Connections` slots are populated.
      */
    val connectionsDeferred: Deferred[IO, Connections] = Deferred.unsafe[IO, Connections]

    override def supervisorStrategy: SupervisionStrategy[IO] =
        OneForOneStrategy[IO](maxNrOfRetries = 3, withinTimeRange = 1.minute) {
            case _: IllegalArgumentException =>
                Escalate // Normally `Stop` but we can't handle stopped actors yet
            case _: RuntimeException =>
                Escalate // Normally `Restart` but our actors can't do that yet
            case _: Exception => Escalate
        }

    override def preStart: IO[Unit] = context.self ! PreStart

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: Request): IO[Unit] = req match {
        case PreStart => preStartLocal
        case TerminatedChild(childType, _) =>
            tracer.traceWith(TerminatedActor(childType))
        case TerminatedDependency(dependencyType, _) =>
            tracer.traceWith(MRMEvent.TerminatedDependency(dependencyType))
        // TODO: Implement a way to receive a remote comm actor and connect it to its corresponding local comm actor
    }

    /** Role-specific boot: spawn the role's full actor set, populate `Connections`, replay state,
      * then complete the `pendingConnections` + [[connectionsDeferred]] barriers.
      */
    protected def preStartLocal: IO[Unit]

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
              BlockWeaver(config, pendingConnections, tracers.blockWeaver)
            )
            cardanoLiaison <- context.actorOf(
              CardanoLiaison(
                config,
                cardanoBackend,
                pendingConnections,
                tracers.cardanoLiaison,
                persistence
              )
            )
            consensusActor <- context.actorOf(
              FastConsensusActor(
                config,
                pendingConnections,
                tracers.fastConsensusActor,
                persistence
              )
            )
            jointLedger <- context.actorOf(
              JointLedger(config, pendingConnections, l2Ledger, tracers.jointLedger, persistence)
            )
            stackComposer <- context.actorOf(
              StackComposer(config, pendingConnections, tracers.stackComposer, persistence)
            )
            slowConsensusActor <- context.actorOf(
              SlowConsensusActor(
                config,
                pendingConnections,
                tracers.slowConsensusActor,
                persistence
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
