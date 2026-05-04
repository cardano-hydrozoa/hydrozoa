package hydrozoa.config.head.multisig.timing

import cats.*
import cats.data.*
import org.scalacheck.Gen
import test.GenWithTestPeers

def generateAnyTxTiming: GenWithTestPeers[TxTiming] =
    ReaderT(network =>
        Gen.oneOf(generateDefaultTxTiming, generateYaciTxTiming, generateTestnetTxTiming)
            .flatMap(_.run(network))
    )

def generateDefaultTxTiming: GenWithTestPeers[TxTiming] =
    ReaderT(network => Gen.const(TxTiming.default(network.slotConfig)))

def generateYaciTxTiming: GenWithTestPeers[TxTiming] =
    ReaderT(network => Gen.const(TxTiming.yaci(network.slotConfig)))

def generateTestnetTxTiming: GenWithTestPeers[TxTiming] =
    ReaderT(network => TxTiming.testnet(network.slotConfig))
