package hydrozoa.config

import scalus.cardano.ledger.value.Coin
import spire.compat.ordering
import spire.implicits.additiveSemigroupOps
import spire.math.UByte


object Assumptions:

    // ===================================
    // Params / constants
    // ===================================

    // Serialized size of ADA-only utxo at the base address (with staking and payment credentials)
    val adaOnlyBaseAddressUtxoBytes: Int = 67

    // Max serialized size of a vote utxo (with/without vote)
    val maxVoteUtxoBytes: Int = 150

    // TODO: use protocol params
    val coinsPerUtxoByte: Coin = Coin.unsafeApply(4310)
    val collateralPercentage: BigDecimal = 1.5

    // ===================================
    // Assumptions
    // ===================================

    val worstCaseFixedFeeFallbackTx = Coin.unsafeApply(200_000)
    val worstCaseVariableFeeFallbackTxPerPeer = Coin.unsafeApply(500_000)

    val worstCaseVoteTxFee = Coin.unsafeApply(900_000)
    val worstCaseTallyTxFee = Coin.unsafeApply(800_000)

    // ===================================
    // Babbage min ada
    // ===================================

    // Babbage min ADA for ADA-only utxo with staking and payment credentials
    val minAdaBabbageAdaOnlyBaseAddressUtxo = calculateBabbageUtxoStorageCost(
      adaOnlyBaseAddressUtxoBytes
    )

    val minAdaBabbageVoteUtxo = calculateBabbageUtxoStorageCost(maxVoteUtxoBytes)

    def calculateBabbageUtxoStorageCost(byteSize: Int): Coin =
        coinsPerUtxoByte.scaleIntegral(160 + byteSize).unsafeToCoin

    // ===================================
    // Minimal collateral
    // ===================================

    // Collateral required for vote tx and tally tx
    val minCollateral: Coin =
        List(worstCaseVoteTxFee, worstCaseTallyTxFee).max
            .scaleFractional(collateralPercentage)
            .unsafeToCoin()

    def calculateWorstCaseFallbackTxFees(numberOfPeers: UByte): Coin =
        (worstCaseVariableFeeFallbackTxPerPeer
            .scaleIntegral(numberOfPeers.toInt) + worstCaseFixedFeeFallbackTx)
            .unsafeToCoin

end Assumptions
