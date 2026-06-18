package hydrozoa.multisig.persistence.codec

import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import io.circe.{Decoder, Encoder}

/** Persistence-layer JSON codec for the value at `StoreKey.RequestHighWater` — JointLedger's
  * per-peer request high-water blob `Map[HeadPeerNumber, RequestNumber]`, rewritten on each own
  * soft-ack. The `ReplayActor` reads it at boot to seed each peer's RequestLane resume cursor
  * (`high-water + 1`, §5.3).
  *
  * `HeadPeerNumber` carries a `KeyEncoder`/`KeyDecoder` and `RequestNumber` a `Codec`, so Circe's
  * automatic `Map` instances suffice — these givens just name them for `StoreCodec.fromCirce`.
  */
object RequestHighWaterCodec:

    import HeadPeerNumber.{headPeerNumberKeyDecoder, headPeerNumberKeyEncoder}
    import RequestNumber.given

    given requestHighWaterEncoder: Encoder[Map[HeadPeerNumber, RequestNumber]] =
        Encoder.encodeMap[HeadPeerNumber, RequestNumber]

    given requestHighWaterDecoder: Decoder[Map[HeadPeerNumber, RequestNumber]] =
        Decoder.decodeMap[HeadPeerNumber, RequestNumber]
