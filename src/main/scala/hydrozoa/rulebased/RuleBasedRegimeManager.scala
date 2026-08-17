package hydrozoa.rulebased

import cats.effect.*
import cats.syntax.foldable.*
import com.suprnation.actor.Actor.*
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.{BlockWeaver, CardanoLiaison, CardanoLiaisonEvent}
import hydrozoa.multisig.persistence.Persistence

/** Supervision boundary between the multisig regime and the rule-based regime: takes the
  * persistence + backend + tracer that the parent hands over, and spawns the [[RuleBasedActor]]
  * that does the work.
  *
  * Two entry paths use this same manager (design `docs/spec/evacuate-command.md`):
  *   - the multisig→rule-based **handoff**, where `HeadMultisigRegimeManager` spawns it after a
  *     fallback lands. There, HMRM keeps its own `CardanoLiaison` alive across the transition (it
  *     is the actor that reacts to an L1 rollback), so this manager spawns **only** the
  *     `RuleBasedActor` — `ownLiaison = None`.
  *   - the standalone **`evacuate`** command, where no multisig `CardanoLiaison` pre-exists. There
  *     `ownLiaison` is `Some(...)`, and this manager also spawns a `CardanoLiaison` so the fallback
  *     gets submitted and in-flight rollouts finish. That liaison re-recovers its full target state
  *     from persistence at boot; its "switch to rule-based" notification is a no-op (there is no
  *     regime manager to hand off to — the `RuleBasedActor` running alongside picks the regime up
  *     directly off L1).
  *
  * The actors themselves own every persistence read and every chain interaction; this shell exists
  * only so the parent can spawn a single "regime manager" child per handoff.
  */
case class RuleBasedRegimeManager(
    cardanoBackend: CardanoBackend[IO],
    persistence: Persistence[IO],
    tracer: ContraTracer[IO, RuleBasedActorEvent],
    ownLiaison: Option[RuleBasedRegimeManager.OwnLiaison],
)(using config: RuleBasedRegimeManager.Config)
    extends Actor[IO, Unit] {

    override def preStart: IO[Unit] =
        for {
            _ <- ownLiaison.traverse_(spawnCardanoLiaison)
            _ <- context.actorOf(
              RuleBasedActor(
                persistence = persistence,
                cardanoBackend = cardanoBackend,
                tracer = tracer
              )
            )
        } yield ()

    /** Spawn the standalone-path `CardanoLiaison` (see [[OwnLiaison]]). Its block-weaver slot is a
      * no-op sink: the liaison forwards L1 head-state to a `BlockWeaver` only in the pre-handoff
      * window, but the rule-based regime runs no `BlockWeaver` and makes no deposit decisions, so
      * dropping those forwards is exactly right.
      */
    private def spawnCardanoLiaison(setup: RuleBasedRegimeManager.OwnLiaison): IO[Unit] =
        for {
            blockWeaverSink <- context.actorOf(RuleBasedRegimeManager.NoOpBlockWeaver)
            _ <- context.actorOf(
              CardanoLiaison(
                config,
                cardanoBackend,
                CardanoLiaison.Connections(blockWeaver = blockWeaverSink),
                setup.tracer,
                persistence,
                // No regime manager to notify — the RuleBasedActor observes the regime off L1
                // itself, so re-announcing would just be spam.
                onRuleBasedRegimeObserved = _ => IO.unit,
                // No `/ready` server on the evacuate path.
                advanceNodeStatus = _ => IO.unit,
              )
            )
        } yield ()
}

object RuleBasedRegimeManager {

    /** The rule-based regime spawns a `CardanoLiaison`, so its config must satisfy the liaison's
      * sections too (both are satisfied by `NodeConfig`).
      */
    type Config = RuleBasedActor.Config & CardanoLiaison.Config

    /** Supplied only on the standalone `evacuate` path (see the class doc): carries what the
      * manager needs to spawn its own `CardanoLiaison`. `None` on the multisig→rule-based handoff,
      * where the multisig `CardanoLiaison` stays alive instead.
      */
    final case class OwnLiaison(tracer: ContraTracer[IO, CardanoLiaisonEvent])

    /** A block-weaver-shaped black hole: absorbs every message and does nothing. Used as the
      * `CardanoLiaison`'s block-weaver slot on the evacuate path (see [[spawnCardanoLiaison]]).
      */
    private object NoOpBlockWeaver extends Actor[IO, BlockWeaver.Request] {
        override def receive: Receive[IO, BlockWeaver.Request] =
            PartialFunction.fromFunction(_ => IO.unit)
    }
}
