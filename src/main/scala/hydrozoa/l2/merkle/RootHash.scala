package hydrozoa.l2.merkle

import hydrozoa.infra.CryptoHash.H32
import hydrozoa.{OutputL2, UtxoIdL2}

type UtxoSetL2 = Map[UtxoIdL2, OutputL2]

type RH32UtxoSetL2 = Unit // H32[UtxoSetL2]

object RH32UtxoSetL2:
    def dummy: RH32UtxoSetL2 = () // H32.hash(IArray()) // TODO: implement
