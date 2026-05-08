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
import hydrozoa.rulebased.ledger.l1.tx.CommonGeneratorsTypes.{genEmptyTreasuryResolvedDatum, Version}
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import monocle.syntax.all.*
import org.scalacheck.{Gen, Properties}
import scalus.cardano.ledger.{Utxo as _, *}
import scalus.uplc.builtin.ByteString
import registry.scalacheck.*

private lazy val deinitGens =
    // Auto-derive the recipe from its case-class fields; everything else below tunes those fields.
    gen[DeinitTx.Build] +:
        // Bound the treasury Coin: the default `arbitrary[Coin]` is unbounded `Long`, which
        // overflows when summed with collateral / change downstream.
        refineGen[Coin](Gen.choose(2_000_000L, 100_000_000L)) +:
        // Make sure that the treasury UTXO's output meets Cardano's per-output min-ADA
        refineGen[DeinitTx.Build](ensureMinAda) +:
        // Use a `Resolved` treasury datum (with an empty trusted setup): deinit only fires
        // post-tally, and the datum body is irrelevant on this path — the validator only checks
        // that all beacon / vote tokens are burned.
        gen(genEmptyTreasuryResolvedDatum) +:
        // Set the treasury value to ADA + 1 beacon + (nPeers+1) vote tokens, matching exactly
        // what the deinit tx must burn.
        gen(genTreasuryValue) +:
        CommonGenerators.gens

def genTreasuryValue(
    config: HeadPeers.Section & HasTokenNames,
    coin: Coin,
): Gen[Value] =
    val headMp = config.headMultisigScript.policyId
    val voteTokensAmount = config.nHeadPeers.toInt + 1
    Gen.const(
      Value(coin)
          + Value.asset(headMp, config.headTokenNames.treasuryTokenName, 1)
          + Value.asset(headMp, config.headTokenNames.voteTokenName, voteTokensAmount)
    )

def ensureMinAda(
    raw: RuleBasedTreasuryUtxo,
    config: HeadPeers.Section & HasTokenNames & CardanoNetwork.Section,
): RuleBasedTreasuryUtxo =
    val outputMinAda = raw.toUtxo(using config).toTuple._2.ensureMinAda(config)
    raw.focus(_.treasuryOutput.value).replace(outputMinAda.value)

object DeinitTxTest extends Properties("Deinit Tx Test") {
    import MultiNodeConfig.*

    val _ = property("Deinit Simple Happy Path") = runDefault {
        // Pin one MultiNodeConfig across both `forAll` samples.
        val gens = deinitGens.const[MultiNodeConfig]
        for {
            nc <- forAll[NodeConfig](gens)
            builder <- forAll[DeinitTx.Build](gens)
            deinitTx <- failLeft(builder.result(using nc))
            _ <- assertWith(deinitTx.tx != null, "Transaction should not be null")
            _ <- assertWith(
              builder.treasuryUtxo == deinitTx.treasuryUtxoSpent,
              "Spent treasury UTXO should match recipe input"
            )
        } yield true
    }
}
