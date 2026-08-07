package hydrozoa.multisig.ledger.eutxol2

import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import scalus.cardano.ledger.ArbitraryInstances.{*, given}
import scalus.cardano.ledger.{TransactionInput, TransactionOutput, Utxos}

object TransientTokensTest extends Properties("TransientTokens") {
    import Prop.{forAll, propBoolean}

    private val genMainUtxos: Gen[Utxos] =
        Gen.nonEmptyMap(Gen.zip(arbitrary[TransactionInput], arbitrary[TransactionOutput]))

    /** Compartments whose overlay keys are a subset of the main keys (the compartment invariant).
      */
    private val genCompartments: Gen[(Utxos, TransientTokens)] =
        for {
            main <- genMainUtxos
            overlaidKeys <- Gen.someOf(main.keys)
            bundles <- Gen.listOfN(overlaidKeys.size, genMultiAsset())
        } yield (main, overlaidKeys.zip(bundles).toMap)

    val _ = property("projectMainUtxos inverts mkCombinedUtxos") = forAll(genCompartments) {
        case (main, transientTokens) =>
            val combined = TransientTokens.mkCombinedUtxos(main, transientTokens)
            TransientTokens.projectMainUtxos(combined, transientTokens) == main
    }

    val _ = property("mkCombinedUtxos adds each bundle to its utxo's assets") =
        forAll(genCompartments) { case (main, transientTokens) =>
            val combined = TransientTokens.mkCombinedUtxos(main, transientTokens)
            combined.keySet == main.keySet &&
            main.forall { case (input, output) =>
                val expectedAssets = transientTokens.get(input) match {
                    case Some(bundle) => output.value.assets + bundle
                    case None         => output.value.assets
                }
                combined(input).value.assets == expectedAssets
            }
        }

    val _ = property("overlay keys outside the main compartment are ignored") =
        forAll(genMainUtxos, arbitrary[TransactionInput], genMultiAsset()) {
            (main, foreignInput, bundle) =>
                !main.contains(foreignInput) ==> {
                    val transientTokens = Map(foreignInput -> bundle)
                    TransientTokens.mkCombinedUtxos(main, transientTokens) == main &&
                    TransientTokens.projectMainUtxos(main, transientTokens) == main
                }
        }
}
