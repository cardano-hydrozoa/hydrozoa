package hydrozoa.config

import scala.math.BigDecimal.RoundingMode.UP
import scalus.cardano.ledger.value.Coin
import spire.math.{SafeLong, UByte}

object Assumptions:

    // ===================================
    // Params / constants
    // ===================================

    // Serialized size of ADA-only utxo with staking and payment credentials
    val adaOnlyBaseAddressUtxoBytes: Int = 67

    // Max serialized size of vote utxo (with/without vote)
    val maxVoteUtxoBytes: Int = 150

    // TODO: use protocol params
    val coinsPerUtxoByte: Coin = Coin.unsafeApply(4310)
    val collateralPercentage: BigDecimal = 1.5f

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

    // TODO: This is why having `scale[I](i: I)(using int Integral[I])` would be nice
    def calculateBabbageUtxoStorageCost(byteSize: Int): Coin =
        coinsPerUtxoByte.scale(SafeLong(160 + byteSize)).unsafeToCoin

    // ===================================
    // Minimal collateral
    // ===================================

    // Collateral required for vote tx and tally tx
    val minCollateral: Coin = Coin.Aggregate
        // TODO: vararg version?
        .max(
            List(
                worstCaseVoteTxFee,
                worstCaseTallyTxFee
            )
        )
        .scale(collateralPercentage)
        .round(UP)
        .unsafeToCoin

    // TODO: here would be nice to get rid of `.toCoinUnbounded
    def calculateWorstCaseFallbackTxFees(numberOfPeers: UByte): Coin =
        worstCaseVariableFeeFallbackTxPerPeer
            // FIXME: is there better way to go UByte -> SafeLong?
            .scale(SafeLong(numberOfPeers.toInt) + worstCaseFixedFeeFallbackTx.toCoinUnbounded)
            .unsafeToCoin

end Assumptions
