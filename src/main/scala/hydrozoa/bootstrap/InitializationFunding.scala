package hydrozoa.bootstrap

import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.multisig.ledger.l1.token.CIP67.HeadTokenNames
import io.bullet.borer.Cbor
import scalus.cardano.ledger.{TransactionInput, TransactionOutput, Utxo, Utxos, Value}
import scalus.uplc.builtin.Builtins.blake2b_224
import scalus.uplc.builtin.ByteString

/** The build-time funding recipe for the head's initialization tx: which utxos fund it, which of
  * them is the seed (whose id determines the head id), and where change goes.
  *
  * This is a bootstrapping concern — the running head never needs it, since it only ever *parses*
  * the finished tx. It carries the pieces that used to live on [[InitializationParameters]] but are
  * pure build inputs.
  */
final case class InitializationFunding(
    seedUtxo: Utxo,
    additionalFundingUtxos: Utxos,
    changeOutputs: List[TransactionOutput]
) {

    /** All funding utxos, seed included. */
    val fundingUtxos: Utxos = additionalFundingUtxos + seedUtxo.toTuple

    /** The head id derived from the seed utxo's input — the head's on-chain identity. */
    val headTokenNames: HeadTokenNames = InitializationFunding.mkHeadTokenNames(seedUtxo.input)

    val headId: InitializationParameters.HeadId =
        InitializationParameters.HeadId(headTokenNames.treasuryTokenName)

    // FIXME: Must be positive
    val seedIx: Int = fundingUtxos.keys.toList.sorted.indexOf(seedUtxo.input)

    /** Total funding value minus change — must equal the unbalanced treasury value. */
    def fundingValue: Value =
        fundingUtxos.values.map(_.value).fold(Value.zero)(_ + _) -
            changeOutputs.map(_.value).fold(Value.zero)(_ + _)

    /** The funding is balanced iff its net value equals the initial L2 value plus the equity and
      * the whole head's fallback contingency.
      */
    def isBalanced(
        config: InitializationParameters.Section & FallbackContingency.Section & HeadPeers.Section
    ): Boolean =
        fundingValue == config.initialL2Value +
            Value(config.initialEquityContributed + config.totalFallbackContingency)
}

object InitializationFunding {

    /** The head-id calculation: hash the CBOR-encoded seed input to the shared CIP-67 suffix. This
      * lives in the bootstrap module because computing the head id is a config-authoring concern;
      * the running head reads the head id from its config instead of re-deriving it.
      */
    def mkHeadTokenNames(seedInput: TransactionInput): HeadTokenNames =
        HeadTokenNames.fromSuffix(
          blake2b_224(ByteString.fromArray(Cbor.encode(seedInput).toByteArray))
        )

    /** Convenience: the head id for a given seed input. */
    def mkHeadId(seedInput: TransactionInput): InitializationParameters.HeadId =
        InitializationParameters.HeadId(mkHeadTokenNames(seedInput).treasuryTokenName)
}
