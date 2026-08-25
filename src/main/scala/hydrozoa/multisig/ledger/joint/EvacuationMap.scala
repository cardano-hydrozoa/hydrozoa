package hydrozoa.multisig.ledger.joint

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.{byteStringDecoder, byteStringEncoder}
import hydrozoa.multisig.ledger.commitment.KzgCommitment
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.EvacuationMap.mkScalar
import hydrozoa.multisig.ledger.joint.EvacuationMapInstances.given
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.remote.RemoteL2LedgerCodecs
import hydrozoa.multisig.ledger.remote.RemoteL2LedgerCodecs.{payoutObligationDecoder, payoutObligationEncoder}
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.given
import io.circe.*
import io.circe.syntax.*
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.immutable.{SortedMap, TreeMap}
import scala.util.Try
import scalus.cardano.ledger.*
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.cardano.onchain.plutus.v2.TxOut
import scalus.uplc.builtin.Builtins.{blake2b_224, serialiseData}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data, ToData, platform}
import scalus.|>
import scodec.bits.ByteVector
import supranational.blst.Scalar

given toDataTransactionInput: ToData[TransactionInput] with {
    override def apply(i: TransactionInput): Data =
        toData(LedgerToPlutusTranslation.getTxOutRefV3(i))
}

given evacuationKeyOrdering: Ordering[EvacuationKey] with {
    override def compare(x: EvacuationKey, y: EvacuationKey): Int =
        summon[Ordering[ByteString]].compare(x.byteString, y.byteString)
}

/** Circe and collection given instances for [[EvacuationKey]].
  *
  * Defined here (not in the companion in `cardano-onchain`) to keep circe out of the on-chain
  * subproject.
  */
object EvacuationMapInstances:
    given Encoder[EvacuationKey] = Encoder.instance { ek =>
        byteStringEncoder(ek.byteString)
    }

    given Decoder[EvacuationKey] = Decoder.instance { c =>
        byteStringDecoder(c).flatMap { bytes =>
            EvacuationKey(bytes) match {
                case Some(key) => Right(key)
                case None      => Left(io.circe.DecodingFailure("Invalid EvacuationKey", c.history))
            }
        }
    }

    given evacuationKeyKeyEncoder: KeyEncoder[EvacuationKey] = {
        // Hex via ByteVector, not `byteString.toHex`: the latter caches its hex on the retained key.
        KeyEncoder.encodeKeyString.contramap(ek => ByteVector(ek.byteString.bytes).toHex)
    }

    // FIXME: This is partial, but KeyDecoder lacks the "emap" method that Decoder has?
    given evacuationKeyKeyDecoder: KeyDecoder[EvacuationKey] with {
        override def apply(s: String): Option[EvacuationKey] =
            for {
                _ <- KeyDecoder.decodeKeyString(s)
                bytes <- Try(ByteString.fromHex(s)).toOption
                ek <- EvacuationKey(bytes)
            } yield ek
    }

/** A digest of an [[EvacuationMap]], as defined in `docs/spec/l2-ledger-command-coordination.md`
  * ("The evacuation map digest").
  *
  * A **coordination check, not an on-chain commitment**: the head commits to its evacuation map on
  * L1 with [[EvacuationMap.kzgCommitment]], which needs BLS12-381 and the head's trusted setup.
  * This digest exists so a remote L2 ledger and the head can cheaply agree that they hold the
  * *same* map, and so is defined over bytes both already have — the raw evacuation key and the
  * entry's raw CBOR output — with no Plutus `Data` encoding on either side.
  *
  * Sugar Rush computes it in `types/src/types/evacuation_map.rs`; the two are pinned to a shared
  * golden, so a change to [[EvacuationMap.digest]] is a wire break.
  */
final case class EvacuationMapHash(byteString: ByteString) {
    // ByteVector, not `byteString.toHex`: the latter caches its hex on this retained digest, and
    // `toString`/the circe encoder both route through here. `bytes` is already in hand.
    def toHex: String = ByteVector(byteString.bytes).toHex

    override def toString: String = toHex
}

object EvacuationMapHash:

    /** Mixed in before anything else so this digest can never collide with a hash of the same bytes
      * taken for another purpose. ASCII, no terminator — the length framing that follows makes the
      * boundary unambiguous.
      */
    val domainTag: Array[Byte] = "gummiworm-evacuation-map-v1".getBytes(UTF_8)

    given Encoder[EvacuationMapHash] =
        Encoder.encodeString.contramap(_.toHex)

    given Decoder[EvacuationMapHash] =
        Decoder.decodeString.emap(s =>
            Try(ByteString.fromHex(s)).toEither.left
                .map(_ => s"not a hex-encoded evacuation map hash: $s")
                .flatMap(bs =>
                    if bs.size == 32 then Right(EvacuationMapHash(bs))
                    else Left(s"evacuation map hash must be 32 bytes, got ${bs.size}")
                )
        )

final case class EvacuationMap(
    evacuationMap: TreeMap[EvacuationKey, Payout.Obligation]
)(using Ordering[EvacuationKey], ToData[EvacuationKey])
    extends SortedMap[EvacuationKey, Payout.Obligation] {
    def iterator: Iterator[(EvacuationKey, Payout.Obligation)] = evacuationMap.iterator

    def removed(key: EvacuationKey): EvacuationMap = EvacuationMap(evacuationMap.removed(key))

    override def removedAll(keys: IterableOnce[EvacuationKey]): EvacuationMap =
        EvacuationMap(evacuationMap.removedAll(keys))

    def updated[V1 >: Payout.Obligation](key: EvacuationKey, value: V1): EvacuationMap =
        EvacuationMap(evacuationMap.updated(key, value.asInstanceOf[Payout.Obligation]))

    // Members declared in scala.collection.MapOps
    def get(key: EvacuationKey): Option[Payout.Obligation] = evacuationMap.get(key)

    /** The evac map, where we threw away the "KeepRaw"
      */
    // Its a silly name, but we use the term "value" too much
    lazy val cooked: TreeMap[EvacuationKey, TransactionOutput] =
        evacuationMap.map((i, obligation) => (i, obligation.utxo.value))
    val outputs: Iterable[Payout.Obligation] = evacuationMap.values

    /** The outputs of the evac map, where we threw away the "KeepRaw"
      */
    lazy val outputsCooked: Iterable[TransactionOutput] = evacuationMap.values.map(_.utxo.value)

    lazy val kzgCommitment: KzgCommitment = KzgCommitment.calculateKzgCommitment(scalars)

    /** The [[EvacuationMapHash]] of this map — see that type for what it is for.
      *
      * ```
      * blake2b_256(
      *      "gummiworm-evacuation-map-v1"
      *   || uint32_be(entryCount)
      *   || for each (key, output) ascending by key:
      *          uint32_be(len(key))    || key
      *       || uint32_be(len(output)) || output
      * )
      * ```
      *
      * Both fields are length-framed because evacuation keys are not fixed-width across L2 ledger
      * backends — the EUTXO ledger keys by the CBOR of a `TransactionInput`, Sugar Rush by a
      * 28-byte account key hash zero-padded to 32 — so an unframed concatenation would be
      * ambiguous. The output contributes its **raw** CBOR, which is why [[Payout.Obligation]] holds
      * a `KeepRaw`: a re-encoding could differ from the bytes the remote hashed.
      *
      * Folding in `evacuationMap`'s own iteration order is what the spec requires: scalus's
      * `Ordering[ByteString]` compares unsigned, byte by byte, shorter prefix first, which is the
      * same order Rust's `Ord` on byte slices gives the remote.
      */
    lazy val digest: EvacuationMapHash = {
        val buffer = ByteArrayOutputStream()
        def putLength(n: Int): Unit = {
            buffer.write((n >>> 24) & 0xff)
            buffer.write((n >>> 16) & 0xff)
            buffer.write((n >>> 8) & 0xff)
            buffer.write(n & 0xff)
        }
        def putFramed(bytes: Array[Byte]): Unit = {
            putLength(bytes.length)
            buffer.write(bytes)
        }
        buffer.write(EvacuationMapHash.domainTag)
        putLength(evacuationMap.size)
        evacuationMap.foreach { (key, obligation) =>
            putFramed(key.byteString.bytes)
            putFramed(obligation.utxo.raw)
        }
        EvacuationMapHash(platform.blake2b_256(ByteString.unsafeFromArray(buffer.toByteArray)))
    }

    lazy val scalars: SList[Scalar] = {
        SList.from(
          evacuationMap.toList.map(e =>
              // FIXME: redundant CBOR encoding with `Sized`, since we're keeping the original serialization anyways
              mkScalar(e._1, LedgerToPlutusTranslation.getTxOutV2(Sized(e._2.utxo.value)))
          )
        )
    }

    /** Assumes key -> value mappings are unique among all maps
      * @return
      */
    def subsetOf(other: EvacuationMap): Boolean =
        evacuationMap.keySet.subsetOf(other.evacuationMap.keySet)

    def totalValue: Value =
        Value.combine(evacuationMap.values.map(_.utxo.value.value))

    override def iteratorFrom(start: EvacuationKey): Iterator[(EvacuationKey, Payout.Obligation)] =
        evacuationMap.iteratorFrom(start)

    override def keysIteratorFrom(start: EvacuationKey): Iterator[EvacuationKey] =
        evacuationMap.keysIteratorFrom(start)

    override def ordering: Ordering[EvacuationKey] = evacuationMap.ordering

    override def rangeImpl(
        from: Option[EvacuationKey],
        until: Option[EvacuationKey]
    ): EvacuationMap =
        EvacuationMap(evacuationMap.rangeImpl(from, until))
}

object EvacuationMap:

    given evacuationMapEncoder: Encoder[EvacuationMap] = {
        Encoder
            .encodeMap[EvacuationKey, Payout.Obligation](using
              evacuationKeyKeyEncoder,
              payoutObligationEncoder
            )
            .contramap(emap => emap.evacuationMap)
    }

    given evacuationMapDecoder(using config: CardanoNetwork.Section): Decoder[EvacuationMap] = {
        Decoder
            .decodeMap[EvacuationKey, Payout.Obligation](using
              evacuationKeyKeyDecoder,
              payoutObligationDecoder
            )
            .map(m => EvacuationMap.from(m))
    }

    def empty: EvacuationMap = EvacuationMap(TreeMap.empty)

    def applyDiffs(evacuationMap: EvacuationMap, diffs: Seq[EvacuationDiff]): EvacuationMap =
        applyDiffsWithDelta(evacuationMap, diffs)._1

    /** Apply `diffs` and return the updated map together with the net change in the map's total
      * value (possibly negative in the coin or an asset). An `Update` contributes its new value
      * minus the overwritten entry's (zero when the key is fresh); a `Delete` subtracts the removed
      * entry's value (zero when the key is absent) — so the delta always equals
      * `result.totalValue - evacuationMap.totalValue`, without an O(map) refold.
      */
    def applyDiffsWithDelta(
        evacuationMap: EvacuationMap,
        diffs: Seq[EvacuationDiff]
    ): (EvacuationMap, Value) =
        diffs.foldLeft((evacuationMap, Value.zero)) { case ((em, delta), diff) =>
            diff match {
                case EvacuationDiff.Update(key, value) =>
                    val overwritten = em.get(key).fold(Value.zero)(_.utxo.value.value)
                    (
                      EvacuationMap(em.evacuationMap.updated(key, value)),
                      delta + value.utxo.value.value - overwritten
                    )
                case EvacuationDiff.Delete(key) =>
                    val removed = em.get(key).fold(Value.zero)(_.utxo.value.value)
                    (EvacuationMap(em.evacuationMap.removed(key)), delta - removed)
            }
        }

    private def mkHash(key: EvacuationKey, output: TxOut): ByteString = {
        (key, output)
            |> ToData.tupleToData
            |> serialiseData
            |> blake2b_224
    }

    def mkScalar(key: EvacuationKey, output: TxOut): Scalar =
        (key, output)
            |> mkHash
            |> (_.bytes)
            |> Scalar().from_bendian

    def from(i: IterableOnce[(EvacuationKey, Payout.Obligation)]): EvacuationMap =
        EvacuationMap(TreeMap.from(i))

enum EvacuationDiff:
    case Update(key: EvacuationKey, value: Payout.Obligation)
    case Delete(key: EvacuationKey)

/** One L2 command's evacuation diffs, with the per-command boundary preserved from the ledger's
  * [[hydrozoa.multisig.ledger.l2.L2LedgerResponse.Applied]] responses (the ledger answers one
  * command at a time). The slow side checks value conservation per group, not per block: a block's
  * aggregate delta can be zero while individual transactions over- and under-credit accounts in
  * compensating directions (e.g. rounding errors on opposite trade directions), so the block-level
  * sum proves nothing about any single transaction.
  */
sealed trait EvacuationDiffGroup {

    /** The group's diffs, in application order. */
    def diffs: Vector[EvacuationDiff]
}

object EvacuationDiffGroup {

    /** An `ApplyTransaction`'s diffs, tagged with the producing request. */
    final case class Transaction(requestId: RequestId, diffs: Vector[EvacuationDiff])
        extends EvacuationDiffGroup

    /** The block's `ApplyDepositDecisions` diffs — the absorbed deposits' spawned L2 genesis
      * outputs (empty for a rejected-only decisions command).
      */
    final case class DepositDecisions(diffs: Vector[EvacuationDiff]) extends EvacuationDiffGroup

    given Encoder[EvacuationDiffGroup] = Encoder.instance {
        case Transaction(requestId, diffs) =>
            io.circe.Json.obj(
              "tag" -> io.circe.Json.fromString("Transaction"),
              "requestId" -> requestId.asJson,
              "diffs" -> diffs.asJson
            )
        case DepositDecisions(diffs) =>
            io.circe.Json.obj(
              "tag" -> io.circe.Json.fromString("DepositDecisions"),
              "diffs" -> diffs.asJson
            )
    }

    given evacuationDiffGroupDecoder(using
        config: CardanoNetwork.Section
    ): Decoder[EvacuationDiffGroup] = {
        Decoder.instance { c =>
            c.downField("tag").as[String].flatMap {
                case "Transaction" =>
                    for {
                        requestId <- c.downField("requestId").as[RequestId]
                        diffs <- c.downField("diffs").as[Vector[EvacuationDiff]]
                    } yield Transaction(requestId, diffs)
                case "DepositDecisions" =>
                    c.downField("diffs").as[Vector[EvacuationDiff]].map(DepositDecisions.apply)
                case other =>
                    Left(
                      io.circe.DecodingFailure(
                        s"Unknown EvacuationDiffGroup tag: $other",
                        c.history
                      )
                    )
            }
        }
    }
}

object EvacuationDiff {

    given Encoder[EvacuationDiff] = Encoder.instance {
        case EvacuationDiff.Update(key, value) =>
            io.circe.Json.obj(
              "tag" -> io.circe.Json.fromString("Update"),
              "key" -> key.asJson,
              "value" -> value.asJson
            )
        case EvacuationDiff.Delete(key) =>
            io.circe.Json.obj(
              "tag" -> io.circe.Json.fromString("Delete"),
              "key" -> key.asJson
            )
    }

    given evacuationDiffDecoder(using config: CardanoNetwork.Section): Decoder[EvacuationDiff] = {
        Decoder.instance { c =>
            c.downField("tag").as[String].flatMap {
                case "Update" =>
                    for {
                        key <- c.downField("key").as[EvacuationKey]
                        value <- c.downField("value").as[Payout.Obligation]
                    } yield EvacuationDiff.Update(key, value)
                case "Delete" =>
                    c.downField("key").as[EvacuationKey].map(EvacuationDiff.Delete.apply)
                case other =>
                    Left(io.circe.DecodingFailure(s"Unknown EvacuationDiff tag: $other", c.history))
            }
        }
    }
}
