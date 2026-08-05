package hydrozoa.multisig.ledger.eutxol2

import hydrozoa.multisig.ledger.eutxol2.L2TxFixtures.*
import hydrozoa.multisig.ledger.eutxol2.tx.L2Tx
import org.scalacheck.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage

/** Conservation properties over randomized mint / burn / pass-through scenarios.
  *
  * The transient-token ("group 3") balance — `overlay_in + mint = declared transients` — has no
  * runtime execution path: with zero fees and no withdrawals it is linearly implied by the full-tx
  * conservation (group 1, combined view) and the main-projection conservation (group 2, main view).
  * These properties pin that implication: every accepted transaction satisfies the group-3
  * equation, and perturbing the declarations by a single token in either direction gets the
  * transaction rejected.
  */
object TransientTokensConservationTest extends Properties("TransientTokens conservation") {
    import Prop.{forAll, propBoolean}

    /** A randomized valid scenario over one 100-ADA pot utxo: `overlayIn` DEMO already overlaid,
      * `mint` more minted (negative = burn, bounded by the overlay), the surviving
      * `overlayIn + mint` tokens split across two L2-bound outputs at a random cut.
      */
    private final case class Scenario(
        overlayIn: Long,
        mint: Long,
        firstOutputTokens: Long
    ) {
        val tokenTotal: Long = overlayIn + mint

        val state: Compartments = {
            val (input, mainOutput) = mkPeerUtxo(42, Value.ada(100))
            val overlay =
                if overlayIn > 0 then Map(input -> mkDemoBundle(overlayIn))
                else TransientTokens.empty
            Compartments(Map(input -> mainOutput), overlay)
        }

        val declarations: Map[Int, MultiAsset] =
            Map(
              0 -> firstOutputTokens,
              1 -> (tokenTotal - firstOutputTokens)
            ).collect { case (index, quantity) if quantity > 0 => index -> mkDemoBundle(quantity) }

        /** Build the signed transaction with the given declarations (the valid ones by default; the
          * perturbation properties pass modified maps over the same outputs).
          */
        def buildWith(declared: Map[Int, MultiAsset]): Transaction = {
            val (input, mainOutput) = state.main.head
            val combinedInput =
                input -> mainOutput.withValue(Value(Coin.ada(100), mkDemoBundle(overlayIn)))
            val sends = List(
              (Babbage(peerAddress, Value(Coin.ada(50), mkDemoBundle(firstOutputTokens))), 2),
              (
                Babbage(
                  peerAddress,
                  Value(Coin.ada(50), mkDemoBundle(tokenTotal - firstOutputTokens))
                ),
                2
              )
            )
            buildSignedL2Tx(
              spends = List(combinedInput),
              sends = sends,
              mints = if mint != 0 then List(mkMintDemoStep(mint)) else Nil,
              transientOutputs = declared
            )
        }
    }

    private val genScenario: Gen[Scenario] =
        for {
            overlayIn <- Gen.choose(0L, 1000L)
            mint <- Gen.choose(-overlayIn, 1000L)
            firstOutputTokens <- Gen.choose(0L, overlayIn + mint)
        } yield Scenario(overlayIn, mint, firstOutputTokens)

    private def applyTransit(
        state: Compartments,
        tx: Transaction
    ): Either[String | TransactionException, (L2Tx, Compartments)] =
        for {
            l2Tx <- L2Tx.parse(tx.toCbor, ledgerConfig)
            next <- HydrozoaTransactionMutator.transit(ledgerConfig, L2TxFixtures.time, state, l2Tx)
        } yield (l2Tx, next)

    private def totalCombinedValue(compartments: Compartments): Value =
        Value.combine(
          TransientTokens
              .mkCombinedUtxos(compartments.main, compartments.transientTokens)
              .values
              .map(_.value)
        )

    val _ = property("accepted transactions satisfy the transient balance and conservation") =
        forAll(genScenario) { scenario =>
            val result = applyTransit(scenario.state, scenario.buildWith(scenario.declarations))
            result match {
                case Left(error) => Prop.falsified :| s"expected success, got: $error"
                case Right((l2Tx, next)) =>
                    val spentInputs = l2Tx.tx.body.value.inputs.toSet
                    val overlayIn = Value.combine(
                      scenario.state.transientTokens
                          .collect {
                              case (input, bundle) if spentInputs(input) =>
                                  Value(Coin.zero, bundle)
                          }
                    )
                    val mint = Value(
                      Coin.zero,
                      l2Tx.tx.body.value.mint.fold(MultiAsset.zero)(m => m)
                    )
                    val declared = Value.combine(
                      l2Tx.transientOutputs.values.map(Value(Coin.zero, _))
                    )
                    val groupThreeBalance = (overlayIn + mint) == declared
                    val overlayMatchesDeclarations =
                        next.transientTokens == l2Tx.mkTransientUtxos
                    val combinedConservation =
                        totalCombinedValue(scenario.state) + mint == totalCombinedValue(next)
                    (s"group-3 balance: $overlayIn + $mint != $declared" |: groupThreeBalance) &&
                    ("overlay != declared entries" |: overlayMatchesDeclarations) &&
                    ("combined value not conserved" |: combinedConservation)
            }
        }

    val _ = property("removing one declared token fails the main projection") =
        forAll(genScenario.suchThat(_.tokenTotal >= 1)) { scenario =>
            val (index, bundle) = scenario.declarations.head
            val reduced = mkDemoBundle(-1) + bundle
            val underDeclared =
                if reduced.isEmpty then scenario.declarations.removed(index)
                else scenario.declarations.updated(index, reduced)
            val result = applyTransit(scenario.state, scenario.buildWith(underDeclared))
            result.left.exists(_.toString.contains("main-projection conservation")) :|
                s"expected an main-projection conservation failure, got: $result"
        }

    val _ = property("adding one declared token exceeds the output's assets") =
        forAll(genScenario) { scenario =>
            val overDeclared = scenario.declarations.updatedWith(0) {
                case Some(bundle) => Some(bundle + mkDemoBundle(1))
                case None         => Some(mkDemoBundle(1))
            }
            val result = applyTransit(scenario.state, scenario.buildWith(overDeclared))
            result.left.exists(_.toString.contains("exceeds the output's assets")) :|
                s"expected a sub-value rejection, got: $result"
        }
}
