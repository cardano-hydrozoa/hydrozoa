package hydrozoa.config.head.multisig.settlement

import hydrozoa.lib.number.PositiveInt
import org.scalacheck.Gen

type SettlementConfigGen = Gen[SettlementConfig]

val generateSettlementConfig: SettlementConfigGen =
    Gen.choose(1, 200).map(i => SettlementConfig(PositiveInt(i).get))
