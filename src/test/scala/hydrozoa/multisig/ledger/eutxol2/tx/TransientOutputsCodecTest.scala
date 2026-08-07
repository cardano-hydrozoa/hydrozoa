package hydrozoa.multisig.ledger.eutxol2.tx

import org.scalacheck.*
import scalus.cardano.ledger.ArbitraryInstances.*
import scalus.cardano.ledger.{Metadatum, MultiAsset}
import scalus.uplc.builtin.ByteString

object TransientOutputsCodecTest extends Properties("TransientOutputs codec") {
    import Prop.forAll

    private val genDeclarations: Gen[Map[Int, MultiAsset]] =
        Gen.mapOf(Gen.zip(Gen.choose(0, 255), genMultiAsset()))

    val _ = property("decodeMetadatum inverts encodeMetadatum") = forAll(genDeclarations) {
        declarations =>
            TransientOutputs.decodeMetadatum(
              TransientOutputs.encodeMetadatum(declarations)
            ) == Right(declarations)
    }

    private def bytesOfLength(n: Int): Metadatum =
        Metadatum.Bytes(ByteString.fromArray(Array.fill(n)(7: Byte)))

    private def bundleWith(
        policyKey: Metadatum = bytesOfLength(28),
        assetKey: Metadatum = bytesOfLength(4),
        quantity: Metadatum = Metadatum.Int(1)
    ): Metadatum =
        Metadatum.Map(Map(policyKey -> Metadatum.Map(Map(assetKey -> quantity))))

    private def declarationsWith(
        bundle: Metadatum,
        index: Metadatum = Metadatum.Int(0)
    ): Metadatum =
        Metadatum.Map(Map(index -> bundle))

    val _ = property("well-formed single declaration decodes") = Prop {
        TransientOutputs.decodeMetadatum(declarationsWith(bundleWith())).isRight
    }

    val _ = property("rejects non-Map metadatum") = Prop {
        List(
          Metadatum.Int(1),
          Metadatum.Text("transient"),
          bytesOfLength(4),
          Metadatum.List(IndexedSeq(Metadatum.Int(1)))
        ).forall(TransientOutputs.decodeMetadatum(_).isLeft)
    }

    val _ = property("rejects negative and non-Int output indices") = Prop {
        List(Metadatum.Int(-1), Metadatum.Text("0"), bytesOfLength(1))
            .map(index => declarationsWith(bundleWith(), index))
            .forall(TransientOutputs.decodeMetadatum(_).isLeft)
    }

    val _ = property("rejects wrong-length policy ids") = Prop {
        List(27, 29)
            .map(n => declarationsWith(bundleWith(policyKey = bytesOfLength(n))))
            .forall(TransientOutputs.decodeMetadatum(_).isLeft)
    }

    val _ = property("rejects over-32-byte asset names") = Prop {
        TransientOutputs
            .decodeMetadatum(declarationsWith(bundleWith(assetKey = bytesOfLength(33))))
            .isLeft
    }

    val _ = property("rejects non-positive quantities") = Prop {
        List(Metadatum.Int(0), Metadatum.Int(-5), Metadatum.Text("1"))
            .map(quantity => declarationsWith(bundleWith(quantity = quantity)))
            .forall(TransientOutputs.decodeMetadatum(_).isLeft)
    }

    val _ = property("rejects empty bundles and empty token maps") = Prop {
        val emptyBundle = declarationsWith(Metadatum.Map(Map.empty))
        val emptyTokens =
            declarationsWith(Metadatum.Map(Map(bytesOfLength(28) -> Metadatum.Map(Map.empty))))
        TransientOutputs.decodeMetadatum(emptyBundle).isLeft &&
        TransientOutputs.decodeMetadatum(emptyTokens).isLeft
    }
}
