package hydrozoa.config.head.initialization

import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.multisig.consensus.peer.PeerNumber
import hydrozoa.multisig.ledger.dapp.token.CIP67.HeadTokenNames
import scalus.cardano.ledger.{Coin, Hash32, TransactionOutput, Utxo, Utxos, Value}

/** Configuration settings for the head's initialization.
  *
  * @param l2Utxos
  *   the utxos with which the head's L2 ledger should be populated upon initialization.
  * @param equityContributions
  *   the ADA amounts (if any) that each peer contributed to the head's equity. The total ADA
  *   contributed must be sufficient for the initialization tx fee, and will also be used for all
  *   subsequent settlement, rollout, and finalization tx fees.
  * @param seedUtxo
  *   among the utxos funding the head's initialization, this utxo's ID determines the head's token
  *   names.
  * @param additionalFundingUtxos
  *   the other funding utxos for initialization, additional to the seed utxo.
  * @param changeOutputs
  *   change outputs that must contain all ADA and non-ADA assets from the funding utxos that are in
  *   excess of the unbalanced treasury value ([[totalEquityContributed]] + [[totalL2Value]]).
  */
final case class InitializationParameters(
    override val l2Utxos: Utxos,
    override val equityContributions: Map[PeerNumber, Coin],
    override val seedUtxo: Utxo,
    override val additionalFundingUtxos: Utxos,
    override val changeOutputs: List[TransactionOutput],
) extends InitializationParameters.Section {
    override transparent inline def initializationParams: InitializationParameters = this

    override lazy val totalL2Value: Value = l2Utxos.values.map(_.value).fold(Value(Coin(0)))(_ + _)

    override lazy val totalEquityContributed: Coin =
        equityContributions.values.fold(Coin(0))(_ + _)

    override lazy val totalFundingValue: Value =
        seedUtxo.output.value +
            additionalFundingUtxos.values.map(_.value).fold(Value(Coin(0)))(_ + _) -
            changeOutputs.map(_.value).fold(Value(Coin(0)))(_ + _)

    /** This is the initial treasury value before deducting the initialization tx fee and adding the
      * minted head token.
      */
    override lazy val unbalancedTreasuryValue: Value = Value(totalEquityContributed) + totalL2Value

    override lazy val headTokenNames: HeadTokenNames = HeadTokenNames(seedUtxo.input)

    // TODO: We need this hash to put into the initialization tx's metadata,
    //  so that the equity contributions are pinned by something signed by all peers.
    override lazy val equityContributionsHash: Hash32 =
        val cbor = ???
        ???
}

object InitializationParameters {
    trait Section {
        def initializationParams: InitializationParameters

        def l2Utxos: Utxos
        def equityContributions: Map[PeerNumber, Coin]
        def seedUtxo: Utxo
        def additionalFundingUtxos: Utxos
        def changeOutputs: List[TransactionOutput]

        def totalEquityContributed: Coin
        def totalFundingValue: Value
        def totalL2Value: Value
        def unbalancedTreasuryValue: Value
        def headTokenNames: HeadTokenNames

        def equityContributionsHash: Hash32
    }

    extension (
        config: InitializationParameters.Section & FallbackContingency.Section & HeadPeers.Section
    )
        def isBalancedInitializationFunding: Boolean =
            config.totalFundingValue ==
                config.unbalancedTreasuryValue + config.multisigRegimeUtxoValue

}
