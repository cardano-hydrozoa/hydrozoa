package hydrozoa.multisig.ledger.l1.utxo

import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.multisig.ledger.l1.utxo.MultisigRegimeOutput.Config
import scalus.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, ReferenceOutput, Send, Spend}

// TODO: Add parsing functions. The Multisig regime utxo should
// carry the correct token and be at the correct address.
final case class MultisigRegimeUtxo(
    input: TransactionInput,
) {

    def toUtxo(using config: MultisigRegimeOutput.Config): Utxo =
        Utxo(
          input,
          MultisigRegimeOutput.toOutput
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

    def toOutput(using config: Config): Babbage = Babbage(
      address = config.headMultisigAddress,
      value = Value(config.totalFallbackContingency) +
          Value.asset(
            config.headMultisigScript.policyId,
            config.headTokenNames.multisigRegimeTokenName,
            1L
          ),
      datumOption = None,
      scriptRef = Some(ScriptRef(config.headMultisigScript.script))
    )

    def burnMultisigRegimeTokens(using config: Config) = Mint(
      config.headMultisigScript.policyId,
      config.headTokenNames.multisigRegimeTokenName,
      -1,
      config.headMultisigScript.witnessAttached
    )

    def send(using config: Config): Send = Send(
      toOutput
    )
}
