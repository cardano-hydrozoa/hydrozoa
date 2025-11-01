package hydrozoa.config

import hydrozoa.config.Assumptions.*
import scalus.cardano.ledger.value.Coin
import spire.compat.ordering
import spire.math.UByte

case class CollectiveContingency(
    fallbackTxFee: Coin,
    defaultVoteDeposit: Coin
)

object CollectiveContingency:
    def apply(numberOfPeers: UByte): CollectiveContingency = {
        CollectiveContingency(
          calculateWorstCaseFallbackTxFees(numberOfPeers),
          minAdaBabbageVoteUtxo
        )
    }

case class IndividualContingency(
    collateralDeposit: Coin,
    voteDeposit: Coin,
    voteTxFee: Coin,
    tallyTxFee: Coin
)

object IndividualContingency:
    def apply: IndividualContingency =
        IndividualContingency(
          List(minCollateral, minAdaBabbageAdaOnlyBaseAddressUtxo).max,
          minAdaBabbageVoteUtxo,
          worstCaseVoteTxFee,
          worstCaseTallyTxFee
        )
