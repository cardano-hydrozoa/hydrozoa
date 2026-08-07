package hydrozoa.multisig.ledger.eutxol2

import hydrozoa.config.head.initialization.InitializationParameters.HeadId.toHex
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import org.scalacheck.Prop.propBoolean
import org.scalacheck.rng.Seed
import org.scalacheck.{Gen, Properties}

/** The headId pin (docs/spec/l2-isomorphism.md): an L2 tx must carry this head's headId in its L2
  * head-label metadata, unless the head runs in identity-isomorphism mode. The pin's presence and
  * well-formedness are enforced by the metadata parse
  * ([[hydrozoa.multisig.ledger.eutxol2.tx.L2Metadata]]); this validator only decides whether the
  * carried headId matches the configured one.
  */
object HeadIdPinTest extends Properties("headId pin") {

    private val headId =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, Seed(0L))
            .headId
    private val expected = headId.toHex

    val _ = property("enforced: matching pin accepted") =
        HeadIdPinValidator.checkPin(identityIsomorphism = false, expected, expected).isRight

    val _ = property("enforced: wrong pin rejected") =
        HeadIdPinValidator.checkPin(identityIsomorphism = false, expected, expected + "00").isLeft

    val _ = property("identity isomorphism: pin not checked") =
        HeadIdPinValidator.checkPin(identityIsomorphism = true, expected, expected + "00").isRight
}
