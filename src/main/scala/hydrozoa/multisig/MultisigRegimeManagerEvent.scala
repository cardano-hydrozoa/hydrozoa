package hydrozoa.multisig

import hydrozoa.multisig.consensus.FastConsensusActorEvent
import hydrozoa.multisig.ledger.joint.JointLedgerEvent

/** Roll-up of every typed event flowing through the multisig regime. One `ContraTracer[IO,
  * MultisigRegimeManagerEvent]` at the MRM level is `contramap`-ped down to per-actor tracers
  * (`JL`, `FCA`, …) inside [[MultisigRegimeManager]]. The wiring layer (`Main` / harness) only has
  * to compose **one** tracer for the whole regime.
  */
sealed trait MultisigRegimeManagerEvent

object MultisigRegimeManagerEvent:
    final case class JL(event: JointLedgerEvent) extends MultisigRegimeManagerEvent
    final case class FCA(event: FastConsensusActorEvent) extends MultisigRegimeManagerEvent
