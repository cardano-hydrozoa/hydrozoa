package hydrozoa.config.head.multisig.timing

import org.scalacheck.Gen
import scalus.cardano.ledger.SlotConfig

type TxTimingGen = SlotConfig => Gen[TxTiming]

def generateDefaultTxTiming = (slotConfig: SlotConfig) => Gen.const(TxTiming.default(slotConfig))

def generateYaciTxTiming = (slotConfig: SlotConfig) => Gen.const(TxTiming.yaci(slotConfig))
