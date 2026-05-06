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
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.genTreasuryValue
import hydrozoa.rulebased.ledger.l1.tx.CommonGeneratorsTypes.{genEmptyTreasuryResolvedDatum, Version}
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import monocle.syntax.all.*
import org.scalacheck.{Gen, Properties}
import scalus.cardano.ledger.{Utxo as _, *}
import scalus.uplc.builtin.ByteString
import registry.scalacheck.*

private lazy val deinitGens =
    gen[DeinitTx.Build] +:
        gen(ensureMinAda) +:
        gen(genEmptyTreasuryResolvedDatum) +:
        gen(genTreasuryValue) +:
        CommonGenerators.gens

def ensureMinAda(
    raw: RuleBasedTreasuryUtxo,
    config: HeadPeers.Section & HasTokenNames & CardanoNetwork.Section,
): RuleBasedTreasuryUtxo =
    val outputMinAda = raw.toUtxo(using config).toTuple._2.ensureMinAda(config)
    raw.focus(_.treasuryOutput.value).replace(outputMinAda.value)

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
