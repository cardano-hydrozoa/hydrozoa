package hydrozoa.integration.stage1

import cats.effect.IO
import com.suprnation.actor.ActorSystem
import hydrozoa.multisig.ledger.JointLedger

/** Stage 1 SUT. */
case class Stage1Sut(
    system: ActorSystem[IO],
    jointLedger: JointLedger.Handle
)
