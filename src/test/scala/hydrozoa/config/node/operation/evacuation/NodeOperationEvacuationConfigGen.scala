package hydrozoa.config.node.operation.evacuation

import hydrozoa.multisig.consensus.peer.HeadPeerWallet
import org.scalacheck.Gen
import scala.concurrent.duration.DurationInt

type NodeOperationEvacuationConfigGen = HeadPeerWallet => Gen[NodeOperationEvacuationConfig]

/** Generate an evacuation bot polling period between 1 and 10 minutes
  */
def generateNodeOperationEvacuationConfig(
    evacuationWallet: HeadPeerWallet
): Gen[NodeOperationEvacuationConfig] =
    for {
        seconds <- Gen.choose(1 * 60, 10 * 60)
    } yield NodeOperationEvacuationConfig(
      evacuationBotPollingPeriod = seconds.seconds,
      evacuationWallet = evacuationWallet
    )
