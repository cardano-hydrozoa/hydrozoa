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
    datum: MultisigRegimeUtxo.Datum
) {

    def toUtxo(using config: MultisigRegimeOutput.Config): Utxo =
        Utxo(
          input,
          MultisigRegimeOutput(datum).toOutput
        )

    def referenceOutput(using config: Config): ReferenceOutput = ReferenceOutput(
      this.toUtxo
    )

    def spend(using config: Config): Spend = Spend(
      this.toUtxo,
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

    /** The datum for a head whose configuration hashes to `headParamsHash`. The digest is passed in
      * rather than read from a config section: the initialization tx builder produces this datum,
      * and it runs before a [[hydrozoa.config.head.HeadConfig]] exists. Everything downstream reads
      * the datum off the [[MultisigRegimeUtxo]] that transaction produced.
      */
    def mkDatum(headParamsHash: Hash32): Datum =
        Datum(ByteString.fromArray(headParamsHash.bytes))

    /** If some tx extends this, it means that tx is producing it. */
    trait Produced {
        def multisigRegimeProduced: MultisigRegimeUtxo
    }

    /** If some tx extends this, it means that tx is spending it. */
    trait Spent {
        def multisigRegimeUtxoSpent: MultisigRegimeUtxo
    }

}

/** The multisig regime output, identified by the datum it carries. Callers that already hold the
  * [[MultisigRegimeUtxo]] go through it instead — this is for the initialization tx builder, which
  * produces the output before any utxo exists.
  */
final case class MultisigRegimeOutput(datum: MultisigRegimeUtxo.Datum) {

    def toOutput(using config: Config): Babbage = Babbage(
      address = config.headMultisigAddress,
      value = Value(config.totalFallbackContingency) +
          Value.asset(
            config.headMultisigScript.policyId,
            config.headTokenNames.regimeWitnessTokenName,
            1L
          ),
      datumOption = Some(Inline(datum.toData)),
      scriptRef = Some(ScriptRef(config.headMultisigScript.script))
    )

    def send(using config: Config): Send = Send(toOutput)
}

object MultisigRegimeOutput {
    type Config = HasTokenNames & CardanoNetwork.Section & HeadPeers.Section &
        FallbackContingency.Section

    def burnRegimeWitnessToken(using config: Config): Mint = Mint(
      config.headMultisigScript.policyId,
      config.headTokenNames.regimeWitnessTokenName,
      -1,
      config.headMultisigScript.witnessAttached
    )
}
