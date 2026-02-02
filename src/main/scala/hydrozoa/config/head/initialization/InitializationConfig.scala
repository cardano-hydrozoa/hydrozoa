package hydrozoa.config.head.initialization

import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.multisig.consensus.peer.PeerNumber
import hydrozoa.multisig.ledger.dapp.token.CIP67.HeadTokenNames
import scalus.cardano.ledger.{Coin, TransactionOutput, Utxo, Utxos, Value}

/** Configuration settings for the head's initialization.
  *
  * @param l2Utxos
  *   the utxos with which the head's L2 ledger should be populated upon initialization.
  * @param l1EquityContributions
  *   the ADA amounts (if any) that each peer contributed to the head's equity. The total ADA
  *   contributed must be sufficient for the initialization tx fee, and will also be used for all
  *   subsequent settlement, rollout, and finalization tx fees.
  * @param l1SeedUtxo
  *   among the utxos funding the head's initialization, this utxo's ID determines the head's token
  *   names.
  * @param l1AdditionalFundingUtxos
  *   the other funding utxos for initialization, additional to the seed utxo.
  * @param l1ChangeOutputs
  *   change outputs that must contain all ADA and non-ADA assets from the funding utxos that are in
  *   excess of the unbalanced treasury value ([[totalEquityContributed]] + [[totalL2Value]]).
  */
final case class InitializationConfig(
    override val l2Utxos: Utxos,
    override val l1EquityContributions: Map[PeerNumber, Coin],
    override val l1SeedUtxo: Utxo,
    override val l1AdditionalFundingUtxos: Utxos,
    override val l1ChangeOutputs: List[TransactionOutput],
) extends InitializationConfig.Section {
    override transparent inline def initializationConfig: InitializationConfig = this

    override lazy val totalL2Value: Value = l2Utxos.values.map(_.value).fold(Value(Coin(0)))(_ + _)

    override lazy val totalEquityContributed: Coin =
        l1EquityContributions.values.fold(Coin(0))(_ + _)

    override lazy val totalFundingValue: Value =
        l1SeedUtxo.output.value +
            l1AdditionalFundingUtxos.values.map(_.value).fold(Value(Coin(0)))(_ + _) -
            l1ChangeOutputs.map(_.value).fold(Value(Coin(0)))(_ + _)

    /** This is the initial treasury value before deducting the initialization tx fee and adding the
      * minted head token.
      */
    override lazy val unbalancedTreasuryValue: Value = Value(totalEquityContributed) + totalL2Value

    override lazy val headTokenNames: HeadTokenNames = HeadTokenNames(l1SeedUtxo.input)
}

object InitializationConfig {
    trait Section {
        def initializationConfig: InitializationConfig

        def l2Utxos: Utxos
        def l1EquityContributions: Map[PeerNumber, Coin]
        def l1SeedUtxo: Utxo
        def l1AdditionalFundingUtxos: Utxos
        def l1ChangeOutputs: List[TransactionOutput]

        def totalEquityContributed: Coin
        def totalFundingValue: Value
        def totalL2Value: Value
        def unbalancedTreasuryValue: Value
        def headTokenNames: HeadTokenNames
    }

    extension (
        config: InitializationConfig.Section & FallbackContingency.Section & HeadPeers.Section
    )
        def isBalancedInitializationConfig: Boolean =
            config.totalFundingValue ==
                config.unbalancedTreasuryValue + config.multisigRegimeUtxoValue

}
