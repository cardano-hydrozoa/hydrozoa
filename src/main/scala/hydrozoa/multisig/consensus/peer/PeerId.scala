package hydrozoa.multisig.consensus.peer

import io.circe.*

/** Identifies the author of a hard-ack: either a head peer (by [[HeadPeerNumber]]) or a coil peer
  * (by [[CoilPeerNumber]]).
  */
enum PeerId {
    case Head(num: HeadPeerNumber)
    case Coil(num: CoilPeerNumber)
}

object PeerId {

    /** Wire form: the peer number shifted left one bit, with the low bit tagging the kind — `1` for
      * head, `0` for coil. Keeps the author id a single integer rather than a structural
      * `{kind, num}` field.
      */
    given Codec[PeerId] = Codec.from(
      Decoder.decodeInt.map(fromWireInt),
      Encoder.encodeInt.contramap(_.toWireInt)
    )

    /** Head peers sort before coil peers; within a kind, by peer number. Gives the signer
      * population a stable total order for deterministic iteration.
      */
    given Ordering[PeerId] with {
        override def compare(x: PeerId, y: PeerId): Int = (x, y) match {
            case (Head(a), Head(b)) => a.convert.compare(b.convert)
            case (Coil(a), Coil(b)) => a.convert.compare(b.convert)
            case (Head(_), Coil(_)) => -1
            case (Coil(_), Head(_)) => 1
        }
    }

    /** Decode the wire integer back into the tagged sum (low bit = kind). */
    def fromWireInt(i: Int): PeerId =
        if (i & 1) == 1 then Head(HeadPeerNumber(i >> 1))
        else Coil(CoilPeerNumber(i >> 1))

    extension (self: PeerId)
        def toWireInt: Int = self match {
            case Head(n) => (n.convert << 1) | 1
            case Coil(n) => n.convert << 1
        }
}
