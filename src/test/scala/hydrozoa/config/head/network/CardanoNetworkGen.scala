package hydrozoa.config.head.network

import org.scalacheck.{Arbitrary, Gen}
import scalus.cardano.ledger.CardanoInfo

object CardanoNetworkGen:
    given Arbitrary[CardanoNetwork] = Arbitrary {
        // A Custom devnet reuses Preview's CardanoInfo with a generated protocolMagic distinct from
        // the standard ones (mainnet 764824073, preprod 1, preview 2).
        val customNetwork: Gen[CardanoNetwork] =
            Gen.choose(3L, Long.MaxValue).map(CardanoNetwork.Custom(CardanoInfo.preview, _))
        Gen.oneOf(
          Gen.const[CardanoNetwork](CardanoNetwork.Mainnet),
          Gen.const[CardanoNetwork](CardanoNetwork.Preprod),
          Gen.const[CardanoNetwork](CardanoNetwork.Preview),
          customNetwork
        )
    }
