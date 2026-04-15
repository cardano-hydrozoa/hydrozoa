package hydrozoa.config.head.multisig.timing

import hydrozoa.config.head.network
import hydrozoa.config.head.network.CardanoNetwork
import org.scalacheck.Gen
import scalus.cardano.ledger.SlotConfig

type TxTimingGen =  Gen[CardanoNetwork.Section ?=> TxTiming]

def generateDefaultTxTiming: TxTimingGen =
    Gen.const((_ : CardanoNetwork.Section) ?=> mkDefaultTxTiming)

def generateYaciTxTiming: TxTimingGen =
    Gen.const((_ : CardanoNetwork.Section) ?=> mkYaciTxTiming)

def generateTestnetTxTiming: TxTimingGen =
    Gen.const((_ : CardanoNetwork.Section) ?=> mkTestnetTxTiming)


def mkDefaultTxTiming(using cardanoNetwork: CardanoNetwork.Section): TxTiming =
    TxTiming.default(cardanoNetwork.slotConfig)

def mkYaciTxTiming(using cardanoNetwork: CardanoNetwork.Section): TxTiming =
    TxTiming.yaci(cardanoNetwork.slotConfig)

def mkTestnetTxTiming(using cardanoNetwork: CardanoNetwork.Section): TxTiming =
    TxTiming.testnet(cardanoNetwork.slotConfig)
