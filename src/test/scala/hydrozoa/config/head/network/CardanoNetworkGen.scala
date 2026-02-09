package hydrozoa.config.head.network

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Arbitrary
import scalus.cardano.ledger.ArbitraryInstances.{given, *}
import scalus.cardano.ledger.CardanoInfo


/**
 * Cannot generate a [[CardanoNetwork.Custom]] because we would
 * need an arbitrary [[CardanoInfo]]
 */
lazy val cardanoNetworkGen : Gen[CardanoNetwork] =
    Gen.oneOf(
          Gen.const(CardanoNetwork.Mainnet),
          Gen.const(CardanoNetwork.Preprod),
          Gen.const(CardanoNetwork.Preview)
    )
