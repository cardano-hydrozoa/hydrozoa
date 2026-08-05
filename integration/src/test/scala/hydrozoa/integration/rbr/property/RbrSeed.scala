package hydrozoa.integration.rbr.property

import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.{Coin, TransactionOutput, Value}

/** The RBR scenario's committed payout obligations — the `PayoutObligations`/`VotableVersions` seed
  * passed to `RBRHlNet.apply`, shared across the property tests that must seed the model with the
  * same obligations the SUT commits on L1.
  */
object RbrSeed:

    /** Version `v` commits `max(1, v)` distinct outputs, distinguished by lovelace so tokens stay
      * distinct; a higher version carries a bigger evacuation batch.
      */
    def committedOutputs(versionMinor: Int): List[TransactionOutput] =
        (1 to versionMinor.max(1)).toList.map { j =>
            TransactionOutput(
              payoutAddress,
              Value(Coin((versionMinor.toLong * 100 + j) * 1_000_000L))
            )
        }

    /** Committed obligations across candidate SEC versions `1..maxVersionMinor`, keyed by version.
      * An empty range (`maxVersionMinor < 1`) is a valid, empty seed.
      */
    def committedObligations(maxVersionMinor: Int): Map[BigInt, List[TransactionOutput]] =
        (1 to maxVersionMinor).map(v => BigInt(v) -> committedOutputs(v)).toMap

    /** The single address every committed payout / evacuation output is sent to. An enterprise
      * **script** address (null delegation): it satisfies `GenesisObligation`'s Shelley/null-stake
      * requirement, and — being script-locked — no peer's key wallet can ever select an evacuation
      * output as fee/collateral (which would consume it and silently drop it from the L1 evacuation
      * count). Shared by the model seed ([[committedOutputs]]) and the SUT deposit generator.
      */
    val payoutAddress: ShelleyAddress =
        Address.fromBech32("addr_test1wqt2v8zcpjldyu2zcwz3yuu8p4wpk0hzaqwthh23qgs5xgg7266qn") match
            case sa: ShelleyAddress => sa
            case other => throw new IllegalStateException(s"payout address must be Shelley: $other")
