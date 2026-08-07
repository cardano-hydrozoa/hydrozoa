package hydrozoa.multisig.ledger.eutxol2.tx

import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen, Properties}
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.{AssetName, KeepRaw, MultiAsset, PolicyId, ScriptHash, Transaction}
import scalus.uplc.builtin.ByteString

/** Round-trip and negative-shape tests for [[L2Metadata]] — the head-label metadata (headId pin,
  * L1-bound output indices, transient-token declarations) every EUTXO L2 transaction carries.
  */
object L2MetadataTest extends Properties("L2 metadata") {

    private val headId: HeadId =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, Seed(0L))
            .headId

    private def withMetadata(baseTx: Transaction, metadata: L2Metadata): Transaction =
        baseTx.copy(auxiliaryData = Some(KeepRaw(L2Metadata.asAuxData(headId, metadata))))

    /** Distinct, sorted indices — the shape `asAuxData` normalizes `l1BoundOutputs` to. */
    private val genL1Bound: Gen[List[Int]] =
        Gen.listOf(Gen.choose(0, 50)).map(_.distinct.sorted)

    val _ = property("round-trips headId + l1BoundOutputs (no transient tokens)") =
        forAll(Arbitrary.arbitrary[Transaction], genL1Bound) { (baseTx, l1Bound) =>
            val metadata = L2Metadata(l1Bound, Map.empty)
            L2Metadata.parse(withMetadata(baseTx, metadata)) == Right((headId, metadata))
        }

    private val demoPolicy: PolicyId =
        ScriptHash.fromByteString(ByteString.fromArray(Array.fill(28)(7.toByte)))
    private val demoBundle: MultiAsset =
        MultiAsset.asset(demoPolicy, AssetName(ByteString.fromString("DEMO")), 5L)

    val _ = property("round-trips l2TransientTokens") = forAll(Arbitrary.arbitrary[Transaction]) {
        baseTx =>
            val metadata = L2Metadata(List(1, 3), Map(1 -> demoBundle))
            L2Metadata.parse(withMetadata(baseTx, metadata)) == Right((headId, metadata))
    }

    val _ = property("rejects a transaction with no auxiliary data") =
        forAll(Arbitrary.arbitrary[Transaction]) { baseTx =>
            L2Metadata.parse(baseTx.copy(auxiliaryData = None)).isLeft
        }
}
