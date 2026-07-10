package hydrozoa.rulebased

import cats.effect.*
import com.suprnation.actor.Actor.*
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.persistence.Persistence

/** Supervision boundary between the multisig regime and the rule-based regime: takes the
  * persistence + backend + tracer that the parent (`HeadMultisigRegimeManager`) hands over on
  * fallback, and spawns the [[RuleBasedActor]] that does the work.
  *
  * The actor itself owns every persistence read and every chain interaction; this shell exists only
  * so the parent regime manager can spawn a single "regime manager" child per handoff, mirroring
  * how it spawns `CoilMultisigRegimeManager` on the coil side.
  */
case class RuleBasedRegimeManager(
    cardanoBackend: CardanoBackend[IO],
    persistence: Persistence[IO],
    tracer: ContraTracer[IO, RuleBasedActorEvent],
)(using config: RuleBasedRegimeManager.Config)
    extends Actor[IO, Unit] {

    override def preStart: IO[Unit] =
        context
            .actorOf(
              RuleBasedActor(
                persistence = persistence,
                cardanoBackend = cardanoBackend,
                tracer = tracer
              )
            )
            .void
}

object RuleBasedRegimeManager {
    type Config = RuleBasedActor.Config
}
