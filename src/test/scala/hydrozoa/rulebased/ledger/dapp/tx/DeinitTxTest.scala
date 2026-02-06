package hydrozoa.rulebased.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.*
import hydrozoa.config.{CollectiveContingency, EquityShares, IndividualContingency}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Tx.Validators.nonSigningValidators
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionValidator.cip67DisputeTokenPrefix
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator.cip67BeaconTokenPrefix
import hydrozoa.rulebased.ledger.dapp.script.plutus.{RuleBasedTreasuryScript, RuleBasedTreasuryValidator}
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.ResolvedDatum
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.RuleBasedTreasuryDatum.Resolved
import hydrozoa.rulebased.ledger.dapp.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.dapp.tx.DeinitTx.{DeinitTxError, Recipe}
import hydrozoa.rulebased.ledger.dapp.utxo.RuleBasedTreasuryUtxo
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scala.annotation.nowarn
import scalus.builtin.ByteString
import scalus.builtin.ByteString.hex
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.{Utxo as _, *}
import scalus.cardano.txbuilder.SomeBuildError
import spire.compat.integral
import spire.math.{Rational, UByte}
import spire.syntax.literals.r
import test.*
import test.Generators.Hydrozoa.genPubkeyAddress

def genEmptyResolvedTreasuryUtxo(
    fallbackTxId: TransactionHash,
    headMp: PolicyId,
    beaconTokenName: AssetName,
    voteTokenName: AssetName,
    voteTokensAmount: Int,
    equity: Coin
): Gen[RuleBasedTreasuryUtxo] = {
    val g1Generator =
        hex"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
    val dummyParams = ByteString.empty
    val dummySetup = scalus.prelude.List.empty

    val emptyResolvedDatum = ResolvedDatum(
      headMp = headMp,
      utxosActive = g1Generator,
      version = (BigInt(1), BigInt(0)),
      params = dummyParams,
      setup = dummySetup
    )

    for {
        outputIx <- Gen.choose(0, 5)
    } yield {
        val txId = TransactionInput(fallbackTxId, outputIx)
        val spp = ShelleyPaymentPart.Script(RuleBasedTreasuryScript.compiledScriptHash)
        val scriptAddr = ShelleyAddress(testNetwork, spp, ShelleyDelegationPart.Null)
        val value = Value(equity) + singleton(headMp, beaconTokenName) + singleton(
          headMp,
          voteTokenName,
          voteTokensAmount
        )

        val treasuryUtxo = RuleBasedTreasuryUtxo(
          treasuryTokenName = beaconTokenName,
          utxoId = txId,
          address = scriptAddr,
          datum = Resolved(emptyResolvedDatum),
          value = value
        )

        // Respect minAda
        val outputMinAda = treasuryUtxo.asTuple._2.ensureMinAda(testProtocolParams)
        treasuryUtxo.copy(value = outputMinAda.value)
    }
}

// TODO: move away from rule-based, it's used in both regimes
/** Generate EquityShares for testing */
def genEquityShares(
    peers: NonEmptyList[TestPeer],
    network: Network = testNetwork
): Gen[EquityShares] =
    for {
        // TODO: shall we use main peers' addresses here?
        addresses <- Gen.listOfN(peers.length, genPubkeyAddress(network))
        shares <- genShares(peers.length)
    } yield {
        val peerShares = addresses
            .zip(shares)
            .zipWithIndex
            .map { case ((addr, share), index) =>
                import spire.math.UByte
                UByte(index) -> (Address[L1](addr), share)
            }
            .toMap

        EquityShares
            .apply(
              shares = peerShares,
              collectiveContingency = CollectiveContingency.apply(UByte(peers.size)),
              individualContingency = IndividualContingency.apply
            )
            .fold(err => throw RuntimeException(s"error generating shares: ${err.explain}"), x => x)
    }

def genRational: Gen[Rational] =
    for {
        den <- Gen.choose(1, 20)
    } yield Rational(1, den)

def genShares(n: Int): Gen[List[Rational]] =
    Gen.frequency(
      1 -> Gen.const(r"1" +: List.fill(n - 1)(r"0")),
      3 -> {
          val share = Rational(1, n)
          List.fill(n)(share)
      },
      5 -> {
          if n == 1 then Gen.const(List(r"1"))
          else
              for {
                  r <- Gen.choose(1, n - 1)
                  randomShares <- Gen.listOfN(r, genRational).suchThat(_.sum <= 1)
              } yield randomShares ++ List.fill(n - (r + 1))(r"0") :+ (r"1" - randomShares.sum)
      }
    )

/** Generate a simplified DeinitTx Recipe for testing */
def genSimpleDeinitTxRecipe: Gen[Recipe] =

    for {
        (hns, headTokenName, peers, peersVks, versionMajor, setupSize, fallbackTxId) <-
            genHeadParams

        // Min treasury
        defaultVoteDeposit = Coin(
          CollectiveContingency.apply(UByte(peers.size)).defaultVoteDeposit.underlying
        )
        voteDeposit = Coin(IndividualContingency.apply.voteDeposit.underlying)
        minTreasury = defaultVoteDeposit + Coin(voteDeposit.value * peers.size)

        equity <- Gen.choose(minTreasury.value, 10000_000_000L).map(Coin(_))

        treasuryUtxo <- genEmptyResolvedTreasuryUtxo(
          fallbackTxId,
          hns.policyId,
          AssetName(cip67BeaconTokenPrefix ++ headTokenName),
          AssetName(cip67DisputeTokenPrefix ++ headTokenName),
          peers.length + 1,
          equity
        )

        shares <- genEquityShares(peers)

        collateralUtxo <- genCollateralUtxo
    } yield {
        Recipe(
          headNativeScript = hns,
          treasuryUtxo = treasuryUtxo,
          defaultVoteDeposit = defaultVoteDeposit,
          voteDeposit = voteDeposit,
          shares = shares,
          collateralUtxo = collateralUtxo,
          evaluator = testEvaluator,
          env = testTxBuilderCardanoInfo,
          validators = nonSigningValidators
        )
    }

@nowarn("msg=unused value")
class DeinitTxTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 100)

    // TODO: this sometimes fails - something is not working stably in our generators
    test("Recipe generator works") {
        val exampleRecipe = genSimpleDeinitTxRecipe.sample.get
        println(exampleRecipe)
    }

    test("DeinitTx builds successfully") {
        forAll(genSimpleDeinitTxRecipe) { recipe =>
            // println(
            //  s"equity: ${recipe.treasuryUtxo.value.coin}, number of shares: ${recipe.shares._2.size}, shares: ${recipe.shares._2
            //          .map(_._2)}"
            // )
            DeinitTx.build(recipe) match {
                case Left(e) =>
                    fail(s"DeinitTx build failed: $e")
                case Right(deinitResult) =>
                    // println(HexUtil.encodeHexString(deinitResult.tx.toCbor))

                    // Verify DeinitTx structure
                    assert(deinitResult.tx != null, "Transaction should not be null")
                    assertResult(
                      recipe.treasuryUtxo,
                      "Spent treasury UTXO should match recipe input"
                    ) {
                        deinitResult.treasuryUtxoSpent
                    }
            }
        }
    }
}
