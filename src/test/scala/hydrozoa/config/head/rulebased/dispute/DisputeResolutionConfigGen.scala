package hydrozoa.config.head.rulebased.dispute

import cats.*
import cats.data.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedFiniteDuration
import org.scalacheck.Gen
import scala.concurrent.duration.DurationInt
import test.{GenWithTestPeers, given}

def generateDisputeResolutionConfig: GenWithTestPeers[DisputeResolutionConfig] =
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
