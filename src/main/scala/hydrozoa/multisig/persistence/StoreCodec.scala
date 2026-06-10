package hydrozoa.multisig.persistence

import hydrozoa.config.head.network.CardanoNetwork
import io.circe.syntax.*
import io.circe.{Decoder, Encoder}

/** Byte codec for a typed [[StoreKey.Value]].
  *
  * Used as the lower bound on every `StoreKey`'s `Value`, so the trait can provide final
  * `encodeValue` / `decodeValue` implementations and individual cases reduce to declaring
  * `type Value = X` plus a `given codec: StoreCodec[Value]`.
  *
  * `CardanoNetwork.Section` is threaded through both methods so codecs that depend on slot config /
  * protocol params (e.g. `QuantizedInstant`, `Payout.Obligation`) can pick it up. The section is
  * captured once at [[Persistence.fromBackend]] time and reaches each codec invocation implicitly —
  * see the [[StoreKey]] docstring.
  */
trait StoreCodec[A]:
    def encode(a: A)(using CardanoNetwork.Section): Array[Byte]
    def decode(bytes: Array[Byte])(using CardanoNetwork.Section): A

object StoreCodec:
    /** Universal Circe-backed instance — any `A` with Circe `Encoder`/`Decoder` (possibly
      * `Section`-dependent) lifts to a `StoreCodec[A]` automatically.
      *
      * The `Section ?=> Encoder[A]` context-function bound is what lets us summon codecs whose
      * `given` definitions take `(using CardanoNetwork.Section)`. At call time the ambient section
      * is forwarded into the factory.
      *
      * Decode failures throw `IllegalArgumentException` — store corruption is treated as fail-fast,
      * matching the previous per-case behavior.
      */
    given fromCirce[A](using
        mkEnc: CardanoNetwork.Section ?=> Encoder[A],
        mkDec: CardanoNetwork.Section ?=> Decoder[A]
    ): StoreCodec[A] with
        def encode(a: A)(using CardanoNetwork.Section): Array[Byte] =
            a.asJson(using mkEnc).noSpaces.getBytes("UTF-8")
        def decode(bytes: Array[Byte])(using CardanoNetwork.Section): A =
            io.circe.parser
                .decode[A](new String(bytes, "UTF-8"))(using mkDec)
                .fold(
                  err => throw new IllegalArgumentException(s"StoreCodec decode failed: $err"),
                  identity
                )

    /** Passthrough for raw bytes — used by every CF whose typed payload hasn't been wired yet.
      * Lives in the companion so the instance is in implicit scope without an import.
      */
    given passthrough: StoreCodec[Array[Byte]] with
        def encode(a: Array[Byte])(using CardanoNetwork.Section): Array[Byte] = a
        def decode(bytes: Array[Byte])(using CardanoNetwork.Section): Array[Byte] = bytes

    /** Lane-CF codec: the Circe (wire-codec) JSON of `A` framed behind the [[ArrivalStamp]] prefix
      * ([[LaneValue]]). Stripping the prefix yields the byte-identical wire form (§7.1).
      *
      * An explicit factory (not a `given`) — each lane `StoreKey` case calls `StoreCodec.laneValue`
      * directly, so there is no implicit ambiguity with [[fromCirce]] and no `summon`
      * self-resolution risk.
      */
    def laneValue[A](using
        mkEnc: CardanoNetwork.Section ?=> Encoder[A],
        mkDec: CardanoNetwork.Section ?=> Decoder[A]
    ): StoreCodec[LaneValue[A]] = new StoreCodec[LaneValue[A]]:
        def encode(lv: LaneValue[A])(using CardanoNetwork.Section): Array[Byte] =
            LaneValue.frame(lv.stamp, lv.payload.asJson(using mkEnc).noSpaces.getBytes("UTF-8"))
        def decode(bytes: Array[Byte])(using CardanoNetwork.Section): LaneValue[A] =
            val a = io.circe.parser
                .decode[A](new String(LaneValue.payload(bytes), "UTF-8"))(using mkDec)
                .fold(
                  err => throw new IllegalArgumentException(s"StoreCodec lane decode failed: $err"),
                  identity
                )
            LaneValue(LaneValue.stamp(bytes), a)
