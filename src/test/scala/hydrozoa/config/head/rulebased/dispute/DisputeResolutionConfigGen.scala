package hydrozoa.config.head.rulebased

import cats.*
import cats.data.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedFiniteDuration
import org.scalacheck.Gen
import scala.concurrent.duration.DurationInt
import test.given

// Not setting a default on purpose. 90% of the time you will want this to
// be coherent with some other slot config, so it's better to force the user
// to pass None explicitly.

type DisputeResolutionConfigGen = ReaderT[Gen, CardanoNetwork.Section, DisputeResolutionConfig]

def generateDisputeResolutionConfig: DisputeResolutionConfigGen =
    for {
        cardanoNetwork <- Kleisli.ask
        // 1 hour to 5 days
        seconds <- ReaderT.liftF(Gen.choose(60 * 60, 60 * 60 * 24 * 5))
    } yield {

        DisputeResolutionConfig(
          votingDuration = QuantizedFiniteDuration(
            slotConfig = cardanoNetwork.slotConfig,
            finiteDuration = seconds.seconds
          )
        )
    }
