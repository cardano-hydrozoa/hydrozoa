package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.network.CardanoNetwork.ensureMinAda
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
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

private lazy val gens =
    gen[DeinitTx.Build] +:
        // override the Unresolved RuleBasedTreasuryUtxo
        gen(genEmptyResolvedTreasuryUtxo) +:
        // pin a single fallbackTxId so the treasury UTxO and other tx-input-derived values agree
        const[TransactionHash] +:
        CommonGenerators.gens

def genEmptyResolvedTreasuryUtxo(
    config: CardanoNetwork.Section & HasTokenNames & HeadPeers.Section,
    fallbackTxId: TransactionHash
): Gen[RuleBasedTreasuryUtxo] = {
    given (CardanoNetwork.Section & HasTokenNames & HeadPeers.Section) = config
    val g1Generator =
        hex"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
    val dummySetup = prelude.List.empty

    val emptyResolvedDatum = Resolved(
      evacuationActive = g1Generator,
      version = (BigInt(1), BigInt(0)),
      setup = dummySetup
    )

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
          emptyResolvedDatum,
          value
        )
        val treasuryUtxo = RuleBasedTreasuryUtxo(
          txId,
          output
        )

        // Respect minAda
        val outputMinAda = treasuryUtxo.toUtxo.toTuple._2.ensureMinAda(config)
        treasuryUtxo.focus(_.treasuryOutput.value).replace(outputMinAda.value)
    }
}

object DeinitTxTest extends Properties("Deinit Tx Test") {
    import MultiNodeConfig.*

    val _ = property("Deinit Simple Happy Path") = runDefault(
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
    )
}
