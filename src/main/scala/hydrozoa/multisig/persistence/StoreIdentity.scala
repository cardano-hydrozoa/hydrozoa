package hydrozoa.multisig.persistence

import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.config.head.initialization.InitializationParameters.HeadId.toHex
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.consensus.peer.PeerId.toWireInt
import java.nio.ByteBuffer
import scalus.cardano.ledger.Hash32

/** What a store belongs to: one head, under one configuration, written by one peer.
  *
  * A store carries no record of any of that today, so pointing a node at the wrong one does not
  * fail — it proceeds on a store that means something else. Worse, `Cf.mkAll` derives the column
  * family set from head and coil membership and
  * [[hydrozoa.multisig.persistence.rocksdb.RocksDbBackendStore]] opens with
  * `setCreateMissingColumnFamilies(true)`, so a store built for a different roster does not even
  * fail at open: the missing per-author families are created **empty** and the node proceeds as
  * though it had no history. A store that reads as empty re-bootstraps from stack 0, the peer can
  * never rejoin the head, and the symptom surfaces far from the cause as an out-of-bounds journal
  * cursor.
  *
  * The stamp is written when a fresh store is initialized and compared on every subsequent open —
  * one point lookup per field, alongside the [[StoreVersion]] check that already runs there.
  *
  * `ownPeerId` is the field that cannot be derived from the configuration, and the one whose
  * absence is most dangerous: which peer a node is comes from `NodePrivateConfig`, so every peer of
  * a head has the identical `headParamsHash`. Opening head peer 0's store as head peer 1 passes
  * every other check while adopting peer 0's own-author journals as this peer's own — the state
  * equivocation avoidance exists to prevent.
  *
  * `headId` is redundant against `headParamsHash` and is stamped anyway, because a bare hash
  * mismatch tells an operator nothing they can act on.
  *
  * See `design/head-params-hash.md`.
  */
final case class StoreIdentity(
    headParamsHash: Hash32,
    headId: HeadId,
    ownPeerId: PeerId
)

object StoreIdentity {

    /** Keys in [[Cf.Meta]], name-keyed UTF-8 like [[StoreVersion.key]]. */
    val headParamsHashKey: Array[Byte] = "head_params_hash".getBytes("UTF-8")
    val headIdKey: Array[Byte] = "head_id".getBytes("UTF-8")
    val ownPeerIdKey: Array[Byte] = "own_peer_id".getBytes("UTF-8")

    extension (self: StoreIdentity) {
        def headParamsHashBytes: Array[Byte] = self.headParamsHash.bytes

        /** The head id as its hex string: self-describing in a store dump, and [[HeadId]] exposes
          * no raw-bytes accessor.
          */
        def headIdBytes: Array[Byte] = self.headId.toHex.getBytes("UTF-8")
        def ownPeerIdBytes: Array[Byte] =
            ByteBuffer.allocate(4).putInt(self.ownPeerId.toWireInt).array()
    }

    /** The three stamped fields, paired with how to read one off a [[StoreIdentity]] and how to
      * render it for an operator. Single-sourced so writing, comparing and reporting cannot drift.
      */
    val fields: List[Field] = List(
      Field("head_params_hash", headParamsHashKey, _.headParamsHashBytes, _.headParamsHash.toHex),
      Field("head_id", headIdKey, _.headIdBytes, _.headId.toHex),
      Field("own_peer_id", ownPeerIdKey, _.ownPeerIdBytes, _.ownPeerId.toWireInt.toString)
    )

    final case class Field(
        name: String,
        key: Array[Byte],
        of: StoreIdentity => Array[Byte],
        render: StoreIdentity => String
    )

    /** Outcome of the open-time identity check, mirroring [[StoreVersion.Check]]. */
    enum Check:
        /** No stamp present — the caller writes the current identity (writable opens only). */
        case Fresh

        /** Every stamped field matches — proceed as normal. */
        case Compatible

        /** At least one field differs, or the stamp is partially written. Refuse to open. */
        case Mismatch(problems: List[String])

    /** Compare a store's stamped fields against `expected`.
      *
      * A partially-written stamp is a mismatch, not a fresh store: writing all three is a single
      * step on a fresh open, so a store missing only some of them is one this build does not
      * understand.
      */
    def check(stamped: Map[String, Array[Byte]], expected: StoreIdentity): Check =
        if fields.forall(f => !stamped.contains(f.name)) then Check.Fresh
        else {
            val problems = fields.flatMap { f =>
                stamped.get(f.name) match {
                    case None =>
                        Some(s"${f.name} is missing from the store's identity stamp")
                    case Some(found) if found.sameElements(f.of(expected)) =>
                        None
                    case Some(found) =>
                        Some(
                          s"${f.name}: the store holds ${renderRaw(f, found)}, " +
                              s"this node has ${f.render(expected)}"
                        )
                }
            }
            if problems.isEmpty then Check.Compatible else Check.Mismatch(problems)
        }

    /** Render a stamped value without decoding it back into its domain type — the stored bytes may
      * be exactly what does not parse.
      */
    private def renderRaw(field: Field, bytes: Array[Byte]): String =
        if field.name == "own_peer_id" && bytes.length == 4 then
            ByteBuffer.wrap(bytes).getInt.toString
        else if field.name == "head_id" then new String(bytes, "UTF-8")
        else bytes.map("%02x".format(_)).mkString
}
