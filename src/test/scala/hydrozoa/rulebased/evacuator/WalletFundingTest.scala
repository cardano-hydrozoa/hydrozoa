package hydrozoa.rulebased.evacuator

import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.uplc.builtin.ByteString

/** Checks the preflight that decides whether an evacuation can start.
  *
  * The property under test is unusual: an evacuation chain is funded by a **single utxo**, not by a
  * wallet balance, so a wallet with ample ada spread thinly cannot pay for it. Getting that wrong
  * is expensive in one direction only — the chain does not fail at launch, it fails partway, having
  * already spent fees and left a treasury part-drained.
  */
class WalletFundingTest extends AnyFunSuite {

    private val env =
        MultiNodeConfig.generateWithCoil().pureApply(Gen.Parameters.default, Seed(0L))

    private given hydrozoa.config.head.network.CardanoNetwork.Section = env.headConfig

    private val params = env.headConfig.cardanoProtocolParams

    private val walletAddress =
        env.nodePrivateConfigs.head._2.ownWallet.exportVerificationKey
            .shelleyAddress()(using env.headConfig)

    // A real policy id from the generated head, rather than a fabricated one: the point is only
    // that the value carries something other than ada.
    private val someToken: MultiAsset =
        Value
            .asset(
              env.headConfig.headMultisigScript.policyId,
              AssetName(ByteString.fromHex("beef")),
              7L
            )
            .assets

    private def utxo(n: Int, ada: Long, tokens: MultiAsset = MultiAsset.empty): Utxo =
        Utxo(
          TransactionInput(TransactionHash.fromHex(f"$n%064x"), 0),
          TransactionOutput.Babbage(
            address = walletAddress,
            value = Value(Coin(ada), tokens),
            datumOption = None,
            scriptRef = None
          )
        )

    private def wallet(us: Utxo*): Utxos =
        us.map(u => u.input -> u.output).toMap

    test("the requirement is the whole chain's fees, not one transaction's") {
        val _ = assert(WalletFunding.required(Coin(1_364_700L), 134).value == 1_364_700L * 134)
    }

    test("a wallet rich in total but thin per utxo cannot fund the chain") {
        // The real case: 1,043 ada across 501 utxos whose largest was 43 — enough for ~30 of 134.
        val feePerTx = Coin(1_364_700L)
        val largest = Coin(43_000_000L)
        val fundable = WalletFunding.fundableTxs(largest, feePerTx)
        val _ = assert(fundable == 31, s"expected ~31 fundable transactions, got $fundable")
        val _ = assert(
          largest.value < WalletFunding.required(feePerTx, 134).value,
          "a 43 ada collateral must not be considered sufficient for 134 transactions"
        )
    }

    test("selection takes ada-only utxos first, so tokens are only touched when necessary") {
        val w = wallet(
          utxo(1, 30_000_000L, someToken),
          utxo(2, 100_000_000L),
          utxo(3, 50_000_000L)
        )
        val plain = WalletFunding.select(w, Coin(120_000_000L))
        val _ = assert(plain.ada.value >= 120_000_000L)
        val _ = assert(
          !plain.hasTokens,
          "a target reachable from ada-only utxos must not drag tokens in"
        )

        val deep = WalletFunding.select(w, Coin(170_000_000L))
        val _ = assert(deep.ada.value >= 170_000_000L)
        val _ =
            assert(deep.hasTokens, "once the plain utxos run out, token-bearing ones must be used")
    }

    test("a target larger than the wallet selects everything and still falls short") {
        val w = wallet(utxo(1, 10_000_000L), utxo(2, 5_000_000L))
        val s = WalletFunding.select(w, Coin(500_000_000L))
        val _ = assert(s.utxos.size == 2)
        val _ = assert(
          s.ada.value == 15_000_000L,
          "the shortfall must be visible, not silently satisfied"
        )
    }

    test("consolidation yields an ada-only output 0 that parses as collateral") {
        val w = wallet(utxo(1, 100_000_000L), utxo(2, 80_000_000L))
        val s = WalletFunding.select(w, Coin(150_000_000L))
        val tx = WalletFunding
            .consolidationTx(s, walletAddress, params)
            .fold(e => fail(s"consolidation did not build: $e"), identity)
        val _ = assert(tx.body.value.outputs.size == 1, "no tokens present, so no token output")
        val collateral = WalletFunding
            .collateralOf(tx)
            .fold(e => fail(s"output 0 unusable as collateral: $e"), identity)
        val _ = assert(collateral.collateralOutput.coin.value > 150_000_000L)
    }

    test("tokens are segregated into their own output, leaving collateral ada-only") {
        val w = wallet(utxo(1, 100_000_000L, someToken), utxo(2, 80_000_000L))
        val s = WalletFunding.select(w, Coin(150_000_000L))
        val _ = assert(s.hasTokens)
        val tx = WalletFunding
            .consolidationTx(s, walletAddress, params)
            .fold(e => fail(s"consolidation did not build: $e"), identity)

        val _ = assert(tx.body.value.outputs.size == 2, "tokens must get an output of their own")
        val out0 = tx.body.value.outputs.head.value
        val out1 = tx.body.value.outputs(1).value
        val _ =
            assert(out0.value.isOnlyAda, "output 0 must stay ada-only or it cannot be collateral")
        val _ = assert(out1.value.assets.assets.nonEmpty, "output 1 must carry the tokens")
        val _ = assert(
          WalletFunding.collateralOf(tx).isRight,
          "the consolidated ada output must be usable as collateral"
        )
    }

    test("NEGATIVE CONTROL: tokens in output 0 make it unusable as collateral") {
        // The segregation above is only meaningful if the alternative genuinely fails. Build the
        // same output but carrying the tokens, and confirm CollateralUtxo rejects it — otherwise
        // the segregation test would pass whether or not the code did anything.
        val polluted = Utxo(
          TransactionInput(TransactionHash.fromHex("00" * 32), 0),
          TransactionOutput.Babbage(
            address = walletAddress,
            value = Value(Coin(200_000_000L), someToken),
            datumOption = None,
            scriptRef = None
          )
        )
        val _ = assert(
          CollateralUtxo.parse(polluted).isLeft,
          "a token-bearing utxo must NOT be accepted as collateral"
        )
        val _ = assert(
          CollateralUtxo.parse(utxo(9, 200_000_000L)).isRight,
          "the ada-only control must pass"
        )
    }
}
