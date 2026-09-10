package hydrozoa.multisig.ledger.l1.utxo

import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.multisig.ledger.l1.utxo.MultisigRegimeOutput.Config
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, ReferenceOutput, Send, Spend}
import scalus.uplc.builtin.Data.{FromData, ToData, toData}
import scalus.uplc.builtin.{ByteString, FromData, ToData}

// TODO: Add parsing functions. The Multisig regime utxo should
// carry the correct token and be at the correct address.
final case class MultisigRegimeUtxo(
    input: TransactionInput,
) {

    def toUtxo(headParamsHash: Hash32)(using config: MultisigRegimeOutput.Config): Utxo =
        Utxo(
          input,
          MultisigRegimeOutput.toOutput(headParamsHash)
        )

    def referenceOutput(headParamsHash: Hash32)(using config: Config): ReferenceOutput =
        ReferenceOutput(
          this.toUtxo(headParamsHash)
        )

    def spend(headParamsHash: Hash32)(using config: Config): Spend = Spend(
      this.toUtxo(headParamsHash),
      config.headMultisigScript.witnessAttached
    )
}

object MultisigRegimeUtxo {

    /** @param headParamsHash
      *   the digest pinning the head's agreed configuration
      *   (`hydrozoa.config.head.HeadParamsHash`). Written once, by the initialization tx, onto the
      *   output whose lifetime is the head's own: the regime utxo is referenced by every deposit
      *   and every settlement and spent only at close, so nothing rewrites this value.
      */
    final case class Datum(
        headParamsHash: ByteString
    ) derives FromData,
          ToData

    /** If some tx extends this, it means that tx is producing it. */
    trait Produced {
        def multisigRegimeProduced: MultisigRegimeUtxo
    }

    /** If some tx extends this, it means that tx is spending it. */
    trait Spent {
        def multisigRegimeUtxoSpent: MultisigRegimeUtxo
    }

}

case object MultisigRegimeOutput {
    type Config = HasTokenNames & CardanoNetwork.Section & HeadPeers.Section &
        FallbackContingency.Section

    /** The head's configuration digest, in the form the regime utxo's inline datum holds it.
      *
      * The digest is passed in rather than read from [[Config]]: computing it needs nearly the
      * whole head config, and the initialization tx builder — the one caller that produces this
      * output — runs before a [[hydrozoa.config.head.HeadConfig]] exists. See
      * `docs/spec/head-params-hash.md`.
      */
    def datum(headParamsHash: Hash32): MultisigRegimeUtxo.Datum =
        MultisigRegimeUtxo.Datum(ByteString.fromArray(headParamsHash.bytes))

    def toOutput(headParamsHash: Hash32)(using config: Config): Babbage = Babbage(
      address = config.headMultisigAddress,
      value = Value(config.totalFallbackContingency) +
          Value.asset(
            config.headMultisigScript.policyId,
            config.headTokenNames.regimeWitnessTokenName,
            1L
          ),
      datumOption = Some(Inline(datum(headParamsHash).toData)),
      scriptRef = Some(ScriptRef(config.headMultisigScript.script))
    )

    def burnRegimeWitnessToken(using config: Config): Mint = Mint(
      config.headMultisigScript.policyId,
      config.headTokenNames.regimeWitnessTokenName,
      -1,
      config.headMultisigScript.witnessAttached
    )

    def send(headParamsHash: Hash32)(using config: Config): Send = Send(
      toOutput(headParamsHash)
    )
}
