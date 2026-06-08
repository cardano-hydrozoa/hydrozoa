package hydrozoa.config.head.coil

import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.multisig.consensus.peer.CoilPeerNumber.given
import hydrozoa.multisig.consensus.peer.HeadPeerNumber.given
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber}
import io.circe.*
import io.circe.generic.semiauto.*
import scala.collection.immutable.SortedMap
import scalus.crypto.ed25519.VerificationKey

/** One coil peer's static data: its verification key and the [[HeadPeerNumber]] of the head peer
  * that hubs it (§8 of `design/coil-network.md`). A coil peer is never dialed — it dials its hub —
  * so it carries no connection address (the hub's address lives in [[HeadPeers]]).
  */
final case class CoilPeerData(
    verificationKey: VerificationKey,
    hubHeadPeerNumber: HeadPeerNumber
)

object CoilPeerData {
    given Encoder[CoilPeerData] = deriveEncoder[CoilPeerData]
    given Decoder[CoilPeerData] = deriveDecoder[CoilPeerData]
}

/** Coil peers keyed by **explicit** [[CoilPeerNumber]]. The number is authoritative — it orders the
  * threshold native script's coil branch and resolves hard-ack signer verification keys — and is
  * NOT derived from verification-key order. Mirrors [[hydrozoa.config.head.peers.HeadPeers]].
  *
  * Invariant: the numbers are contiguous from 0, or the map is empty (a head may hub no coil
  * peers).
  */
final case class CoilPeers private (coilPeerData: SortedMap[CoilPeerNumber, CoilPeerData]) {

    /** Coil peer verification keys in [[CoilPeerNumber]] order (the map's key order). A key's
      * position in this list equals its coil peer number.
      */
    def verificationKeys: List[VerificationKey] =
        coilPeerData.values.map(_.verificationKey).toList

    /** This coil peer's verification key, or `None` if the number is out of range. */
    def verificationKey(p: CoilPeerNumber): Option[VerificationKey] =
        coilPeerData.get(p).map(_.verificationKey)

    /** The hub head peer for the given coil peer, or `None` if the number is out of range. */
    def hubHeadPeerNumber(p: CoilPeerNumber): Option[HeadPeerNumber] =
        coilPeerData.get(p).map(_.hubHeadPeerNumber)

    /** The coil peers hubbed by the given head peer, by [[CoilPeerNumber]]. */
    def hubbedBy(headNum: HeadPeerNumber): List[CoilPeerNumber] =
        coilPeerData.collect { case (n, d) if d.hubHeadPeerNumber == headNum => n }.toList

    /** The distinct hub head peers across all coil peers. */
    def hubHeadPeerNumbers: List[HeadPeerNumber] =
        coilPeerData.values.map(_.hubHeadPeerNumber).toList.distinct

    def size: Int = coilPeerData.size

    def isEmpty: Boolean = coilPeerData.isEmpty
}

object CoilPeers {

    val empty: CoilPeers = new CoilPeers(SortedMap.empty)

    private def isContiguousFromZero(ns: Iterable[CoilPeerNumber]): Boolean = {
        val sorted = ns.map(_.convert).toList.sorted
        sorted == (0 until sorted.size).toList
    }

    /** Build from a number→data map; `None` unless the numbers are contiguous from 0 (or empty). */
    def apply(coilPeerData: SortedMap[CoilPeerNumber, CoilPeerData]): Option[CoilPeers] =
        Option.when(isContiguousFromZero(coilPeerData.keys))(new CoilPeers(coilPeerData))

    /** Build from an ordered list, assigning numbers `0..n-1` by position. */
    def indexed(data: List[CoilPeerData]): CoilPeers =
        new CoilPeers(SortedMap.from(data.zipWithIndex.map((d, i) => CoilPeerNumber(i) -> d)))

    given Encoder[CoilPeers] =
        Encoder.encodeMap[CoilPeerNumber, CoilPeerData].contramap(_.coilPeerData)

    given Decoder[CoilPeers] =
        Decoder
            .decodeMap[CoilPeerNumber, CoilPeerData]
            .emap(m =>
                CoilPeers(SortedMap.from(m))
                    .toRight("coil peer numbers must be contiguous from 0 (or empty)")
            )
}
