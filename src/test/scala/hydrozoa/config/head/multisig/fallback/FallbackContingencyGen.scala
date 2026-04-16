package hydrozoa.config.head.multisig.fallback

import cats.data.ReaderT
import org.scalacheck.Gen
import scalus.cardano.ledger.Coin
import test.GenWithTestPeers

def generateFallbackContingency: GenWithTestPeers[FallbackContingency] = {
    ReaderT(cardanoNetwork =>
        Gen.const(
          cardanoNetwork.mkFallbackContingencyWithDefaults(
            tallyTxFee = Coin.ada(3),
            voteTxFee = Coin.ada(3)
          )
        )
    )
}
