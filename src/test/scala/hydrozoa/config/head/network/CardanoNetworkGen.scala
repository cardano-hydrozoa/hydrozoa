package hydrozoa.config.head.network

import org.scalacheck.Gen

/** Cannot generate a [[CardanoNetwork.Custom]] because we would need an arbitrary [[CardanoInfo]]
  */
lazy val cardanoNetworkGen: Gen[CardanoNetwork] =
    Gen.oneOf(
      Gen.const(CardanoNetwork.Mainnet),
      Gen.const(CardanoNetwork.Preprod),
      Gen.const(CardanoNetwork.Preview)
    )
