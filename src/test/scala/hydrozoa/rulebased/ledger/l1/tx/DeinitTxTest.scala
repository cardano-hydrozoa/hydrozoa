package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.network.CardanoNetwork.ensureMinAda
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Resolved
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.{gens as _, *}
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import monocle.syntax.all.*
import org.scalacheck.{Gen, Properties}
import scalus.cardano.ledger.{Utxo as _, *}
import scalus.cardano.onchain.plutus.prelude
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex
import registry.scalacheck.*

private lazy val deinitGens =
    gen[DeinitTx.Build] +:
        gen(genEmptyResolvedTreasuryUtxo) +:
        // pin a single fallbackTxId so the treasury UTxO and other tx-input-derived values agree
        const[TransactionHash] +:
        gen(genTreasuryResolvedDatum) +:
        CommonGenerators.gens

def genEmptyResolvedTreasuryUtxo(
    resolvedDatum: RuleBasedTreasuryDatum,
    config: CardanoNetwork.Section & HasTokenNames & HeadPeers.Section,
    fallbackTxId: TransactionHash
): Gen[RuleBasedTreasuryUtxo] = {
    val voteTokensAmount = config.nHeadPeers.toInt + 1
    val headMp = config.headMultisigScript.policyId
    val beaconTokenName = config.headTokenNames.treasuryTokenName
    val voteTokenName = config.headTokenNames.voteTokenName

    for {
        outputIx <- Gen.choose(0, 5)
    } yield {
        val txId = TransactionInput(fallbackTxId, outputIx)
        val value = Value(config.babbageUtxoMinLovelace(PositiveInt.unsafeApply(150)))
            + Value.asset(headMp, beaconTokenName, 1)
            + Value.asset(headMp, voteTokenName, voteTokensAmount)

        val output = RuleBasedTreasuryOutput(
            resolvedDatum,
          value
        )
        val treasuryUtxo = RuleBasedTreasuryUtxo(
          txId,
          output
        )

        // Respect minAda
        val outputMinAda = treasuryUtxo.toUtxo(using config).toTuple._2.ensureMinAda(config)
        treasuryUtxo.focus(_.treasuryOutput.value).replace(outputMinAda.value)
    }
}

object DeinitTxTest extends Properties("Deinit Tx Test") {
    import MultiNodeConfig.*

    val _ = property("Deinit Simple Happy Path") = runDefault(
      for {
          nc <- forAll[NodeConfig](deinitGens)
          builder <- forAll[DeinitTx.Build](deinitGens)
          deinitTx <- failLeft(builder.result(using nc))
          _ <- assertWith(deinitTx.tx != null, "Transaction should not be null")
          _ <- assertWith(
            builder.treasuryUtxo == deinitTx.treasuryUtxoSpent,
            "Spent treasury UTXO should match recipe input"
          )
      } yield true
    )
}
