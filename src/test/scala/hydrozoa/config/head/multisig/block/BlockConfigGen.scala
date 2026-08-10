package hydrozoa.config.head.multisig.block

import hydrozoa.lib.number.PositiveInt
import org.scalacheck.Gen

type BlockConfigGen = Gen[BlockConfig]

val generateBlockConfig: BlockConfigGen =
    Gen.choose(1, 2000).map(i => BlockConfig(PositiveInt(i).get))
