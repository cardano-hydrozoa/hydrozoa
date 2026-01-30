package hydrozoa.integration.stage1

import com.suprnation.actor.ActorSystem
import cats.effect.IO
import hydrozoa.multisig.ledger.JointLedger

/** Stage 1 SUT. */
case class Stage1Sut(
    system: ActorSystem[IO],
    jointLedger: JointLedger.Handle
)
