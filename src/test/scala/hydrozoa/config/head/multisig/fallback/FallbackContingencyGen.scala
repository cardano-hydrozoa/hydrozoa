package hydrozoa.config.head.multisig.fallback

import hydrozoa.config.head.network.CardanoNetwork
import org.scalacheck.Gen
import scalus.cardano.ledger.Coin

type FallbackContingencyGen =  Gen[CardanoNetwork.Section ?=> FallbackContingency]

/** TODO: Improve?
  */
def mkFallbackContingency(using cardanoNetwork: CardanoNetwork.Section): FallbackContingency =
      cardanoNetwork.mkFallbackContingencyWithDefaults(
        tallyTxFee = Coin.ada(3),
        voteTxFee = Coin.ada(3)
    )

def generateFallbackContingency : FallbackContingencyGen =
    (_ : CardanoNetwork.Section) ?=> mkFallbackContingency