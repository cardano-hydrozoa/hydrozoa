package hydrozoa.config.head.multisig.fallback

import hydrozoa.config.head.network.CardanoNetwork
import org.scalacheck.Gen
import scalus.cardano.ledger.Coin

type FallbackContingencyGen = CardanoNetwork => Gen[FallbackContingency]

/** TODO: Improve?
  */
def generateFallbackContingency(cardanoNetwork: CardanoNetwork): Gen[FallbackContingency] =
    Gen.const(
      cardanoNetwork.mkFallbackContingencyWithDefaults(
        tallyTxFee = Coin.ada(3),
        voteTxFee = Coin.ada(3)
      )
    )
