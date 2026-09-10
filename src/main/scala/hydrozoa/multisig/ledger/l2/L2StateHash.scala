package hydrozoa.multisig.ledger.l2

import io.circe.{Decoder, Encoder}
import scala.util.Try
import scalus.uplc.builtin.ByteString
import scodec.bits.ByteVector

/** A digest over the elements that constitute an L2 ledger's state, as defined in
  * `design/l2-state-certificate.md`.
  *
  * It is the value a head **certifies**: the settlement's treasury datum and the standalone
  * evacuation commitment each carry one beside their evacuation-map commitment, so an effect plus
  * its N-of-N hard-ack signatures is a signed statement of the state that effect's partition ends
  * at. A peer with no history can be handed one and check it against the head peer verification
  * keys it already holds.
  *
  * **The construction is the backend's, not this type's.** Unlike
  * [[hydrozoa.multisig.ledger.joint.EvacuationMapHash]] — one digest both a Scala head and a Rust
  * sidecar compute over the same wire bytes — this ranges over a ledger's *own* state
  * representation, which the two backends do not share. `l2Ledger: L2LedgerKind` is a head
  * parameter pinned in `headParamsHash`, so every peer in one head drives the same backend and only
  * ever compares digests with peers computing them the same way. Each backend therefore defines its
  * own domain tag and its own fold —
  * [[hydrozoa.multisig.ledger.eutxol2.store.L2Snapshot.stateHash]] for the built-in ledger — and
  * this type carries only the 32 bytes and their encodings.
  */
final case class L2StateHash(byteString: ByteString) {
    // ByteVector, not `byteString.toHex`: the latter caches its hex on this retained digest, and
    // `toString`/the circe encoder both route through here. `bytes` is already in hand.
    def toHex: String = ByteVector(byteString.bytes).toHex

    override def toString: String = toHex
}

object L2StateHash:

    given Encoder[L2StateHash] =
        Encoder.encodeString.contramap(_.toHex)

    given Decoder[L2StateHash] =
        Decoder.decodeString.emap(s =>
            Try(ByteString.fromHex(s)).toEither.left
                .map(_ => s"not a hex-encoded L2 state hash: $s")
                .flatMap(bs =>
                    if bs.size == 32 then Right(L2StateHash(bs))
                    else Left(s"L2 state hash must be 32 bytes, got ${bs.size}")
                )
        )
