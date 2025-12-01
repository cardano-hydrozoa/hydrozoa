package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.dapp.tx.FallbackTx
import org.scalacheck.Gen
import scalus.cardano.ledger.*
import test.*
import test.Generators.Hydrozoa.{genScriptAddress, genTreasuryUtxo, genTxBuilderConfigAndPeers}

// NOTE: This generator isn't currently used. It makes more sense to test this transaction
// as part of its transaction sequence. This generator is provided in case there are bugs
// discovered and we want to isolate testing specifically to this transaction.
// See InitializationTxSeqTest.scala
val genFallbackTxRecipe: Gen[(FallbackTx.Recipe, NonEmptyList[TestPeer])] =
    for {
        (config, peers) <- genTxBuilderConfigAndPeers()
        treasuryUtxo <- genTreasuryUtxo(config)
    } yield (
      FallbackTx.Recipe(
        config = config,
        treasuryUtxo = treasuryUtxo,
        tallyFeeAllowance = Coin(1_000_000L),
        votingDuration = 1L
      ),
      peers
    )
