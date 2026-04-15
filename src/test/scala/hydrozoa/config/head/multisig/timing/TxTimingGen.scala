package hydrozoa.config.head.multisig.timing

import cats.*
import cats.data.*
import hydrozoa.config.head.network
import hydrozoa.config.head.network.CardanoNetwork
import org.scalacheck.Gen

type TxTimingGen = ReaderT[Gen, CardanoNetwork.Section, TxTiming]

def generateDefaultTxTiming: TxTimingGen = ReaderT(network => Gen.const(mkDefaultTxTiming(network)))

def generateYaciTxTiming: TxTimingGen =
    ReaderT(network => Gen.const(mkYaciTxTiming(network)))

def generateTestnetTxTiming: TxTimingGen =
    ReaderT(network => mkTestnetTxTiming(network))

def mkDefaultTxTiming: Reader[CardanoNetwork.Section, TxTiming] =
    Reader(cardanoNetwork => TxTiming.default(cardanoNetwork.slotConfig))

def mkYaciTxTiming: Reader[CardanoNetwork.Section, TxTiming] =
    Reader(cardanoNetwork => TxTiming.yaci(cardanoNetwork.slotConfig))

def mkTestnetTxTiming: Reader[CardanoNetwork.Section, TxTiming] =
    Reader(cardanoNetwork => TxTiming.testnet(cardanoNetwork.slotConfig))
