package hydrozoa.multisig.persistence.codec

import hydrozoa.multisig.consensus.ack.HardAckNumber
import hydrozoa.multisig.consensus.peer.CoilPeerNumber
import io.circe.{Decoder, Encoder}

/** Persistence-layer JSON codec for the value at `StoreKey.CoilStampMark` — a hub's per-coil-peer
  * stamped-high-water blob `Map[CoilPeerNumber, HardAckNumber]`, rewritten on each `HubHardAck`
  * stamp. `CoilAckSequencer.recover` reads it to find the durable coil hard-acks (the coil's
  * `HardAck` receive copy) still needing stamping after a crash (§6).
  *
  * `CoilPeerNumber` carries a `KeyEncoder`/`KeyDecoder` and `HardAckNumber` a `Codec`, so Circe's
  * automatic `Map` instances suffice — these givens just name them for `StoreCodec.fromCirce`.
  */
object CoilStampMarkCodec:

    import CoilPeerNumber.{coilPeerNumberKeyDecoder, coilPeerNumberKeyEncoder}
    import HardAckNumber.given

    given coilStampMarkEncoder: Encoder[Map[CoilPeerNumber, HardAckNumber]] =
        Encoder.encodeMap[CoilPeerNumber, HardAckNumber]

    given coilStampMarkDecoder: Decoder[Map[CoilPeerNumber, HardAckNumber]] =
        Decoder.decodeMap[CoilPeerNumber, HardAckNumber]
