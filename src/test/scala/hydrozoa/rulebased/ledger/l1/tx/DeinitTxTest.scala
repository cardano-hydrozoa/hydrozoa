package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.network.CardanoNetwork.ensureMinAda
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Resolved
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import monocle.*
import monocle.syntax.all.*
import org.scalacheck.{Arbitrary, Gen, Properties}
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.{Utxo as _, *}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex
import spire.compat.integral
import spire.math.Rational
import spire.syntax.literals.r

def genEmptyResolvedTreasuryUtxo(
    fallbackTxId: TransactionHash,
    voteTokensAmount: Int
)(using
    config: CardanoNetwork.Section & HasTokenNames & HeadPeers.Section
): Gen[RuleBasedTreasuryUtxo] = {
    val g1Generator =
        hex"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
    val dummyParams = ByteString.empty
    val dummySetup = scalus.cardano.onchain.plutus.prelude.List.empty

    val emptyResolvedDatum = Resolved(
      evacuationActive = g1Generator,
      version = (BigInt(1), BigInt(0)),
      setup = dummySetup
    )

    val headMp = config.headMultisigScript.policyId
    val beaconTokenName = config.headTokenNames.treasuryTokenName
    val voteTokenName = config.headTokenNames.voteTokenName

    for {
        outputIx <- Gen.choose(0, 5)
    } yield {
        val txId = TransactionInput(fallbackTxId, outputIx)
        val scriptAddr = HydrozoaBlueprint.mkTreasuryAddress(config.network)
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
        treasuryUtxo.focus(_.treasuryOutput.value).set(outputMinAda.value)
    }
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
// FIXME: The arguments to this function can be simplified once the DeinitTx itself is updated to use the new
// configuration
def genSimpleDeinitTxBuilder(using
    config: CardanoNetwork.Section & HeadPeers.Section & HasTokenNames
): Gen[DeinitTx.Build] =

    for {
        fallbackTxId <- Arbitrary.arbitrary[TransactionHash]
        treasuryUtxo <- genEmptyResolvedTreasuryUtxo(
          fallbackTxId,
          config.nHeadPeers.toInt + 1
        )
        akh <- Arbitrary.arbitrary[AddrKeyHash]
        collateralUtxo <- genCollateralUtxo(akh)(using config)

    } yield {
        DeinitTx.Build(
          treasuryUtxo = treasuryUtxo,
          collateralUtxo = collateralUtxo
        )
    }

object DeinitTxTest extends Properties("Deinit Tx Test") {
    import MultiNodeConfig.*

    val _ = property("Deinit Simple Happy Path") = runDefault(
      for {
          env <- ask
          _ <- {
              given MultiNodeConfig = env
              for {
                  builder <- pick(genSimpleDeinitTxBuilder)
                  deinitTx <- failLeft(builder.result(using env.nodeConfigs.head._2))
                  _ <- assertWith(deinitTx.tx != null, "Transaction should not be null")
                  _ <- assertWith(
                    builder.treasuryUtxo == deinitTx.treasuryUtxoSpent,
                    "Spent treasury UTXO should match recipe input"
                  )
              } yield ()
          }
      } yield true
    )
}
