package hydrozoa.config.head.coil

import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.peer.HeadPeerNumber.given
import io.circe.*
import io.circe.generic.semiauto.*
import scalus.crypto.ed25519.VerificationKey

// Question: Do we need CoilPeerNumber? I'd assume so -- we need to have a static ordering for the purposes
//   of the head native script, and I don't think we should rely on json list ordering.
//   Should we disambiguate the serialized form with `HEAD` and `COIL` prefixes?
case class CoilPeer(vkey: VerificationKey, hub: HeadPeerNumber)

object CoilPeer {
    given Encoder[CoilPeer] = deriveEncoder[CoilPeer]
    given Decoder[CoilPeer] = deriveDecoder[CoilPeer]
}
