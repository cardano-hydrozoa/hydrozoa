package hydrozoa.config.head.multisig.fallback

import cats.data.{Reader, ReaderT}
import hydrozoa.config.head.network.CardanoNetwork
import org.scalacheck.Gen
import scalus.cardano.ledger.Coin

type FallbackContingencyGen = ReaderT[Gen, CardanoNetwork.Section, FallbackContingency]

/** TODO: Improve?
  */
def mkFallbackContingency: Reader[CardanoNetwork.Section, FallbackContingency] =
    Reader(cardanoNetwork =>
        cardanoNetwork.mkFallbackContingencyWithDefaults(
          tallyTxFee = Coin.ada(3),
          voteTxFee = Coin.ada(3)
        )
    )

def generateFallbackContingency: FallbackContingencyGen = {
    ReaderT(network => Gen.const(mkFallbackContingency(network)))
}
