package hydrozoa.multisig.ledger.eutxol2

import hydrozoa.config.head.initialization.InitializationParameters.HeadId.toHex
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen, Properties}
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.{KeepRaw, Transaction}

/** The headId pin (§5.2 of the L2-isomorphism design note): an L2 tx must carry this head's headId
  * metadatum, unless the head runs in identity-isomorphism mode.
  */
object HeadIdPinTest extends Properties("headId pin") {

    private val headId =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, Seed(0L))
            .headId
    private val expected = headId.toHex

    val _ = property("enforced: matching pin accepted") =
        HeadIdPinValidator.checkPin(identityIsomorphism = false, expected, Some(expected)).isRight

    val _ = property("enforced: wrong pin rejected") = HeadIdPinValidator
        .checkPin(identityIsomorphism = false, expected, Some(expected + "00"))
        .isLeft

    val _ = property("enforced: missing pin rejected") =
        HeadIdPinValidator.checkPin(identityIsomorphism = false, expected, None).isLeft

    val _ = property("identity isomorphism: pin not checked") =
        HeadIdPinValidator.checkPin(identityIsomorphism = true, expected, None).isRight &&
            HeadIdPinValidator
                .checkPin(identityIsomorphism = true, expected, Some(expected + "00"))
                .isRight

    val _ = property("extract round-trips the headId metadatum; absent aux data is None") =
        forAll(Arbitrary.arbitrary[Transaction]) { baseTx =>
            val withPin =
                baseTx.copy(auxiliaryData =
                    Some(KeepRaw(Metadata(Map(HeadIdPin.metadatum(headId)))))
                )
            (HeadIdPin.extract(withPin) == Some(expected)) &&
            HeadIdPin.extract(baseTx.copy(auxiliaryData = None)).isEmpty
        }
}
