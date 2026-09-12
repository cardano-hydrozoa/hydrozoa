package hydrozoa.multisig.ledger.eutxol2.store

import hydrozoa.multisig.ledger.eutxol2.tx.L2Genesis
import hydrozoa.multisig.ledger.eutxol2.tx.given
import hydrozoa.multisig.ledger.eutxol2.{EutxoL2Ledger, TransientTokens}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l2.{L2CommandNumber, L2StateHash}
import io.bullet.borer.Cbor
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import scalus.cardano.ledger.{MultiAsset, TransactionInput, TransactionOutput, Utxos}
import scalus.uplc.builtin.{ByteString, platform}

/** The **recoverable subset** of [[EutxoL2Ledger.State]] persisted in a snapshot (§R2b).
  *
  * Only `commandNumber` + `activeUtxos` + `transientTokens` + `pendingDeposits` are kept — the
  * fields a recovered ledger needs to resume producing blocks, plus its commit commandNumber.
  */
final case class L2Snapshot(
    commandNumber: L2CommandNumber,
    activeUtxos: Utxos,
    transientTokens: TransientTokens,
    pendingDeposits: Map[RequestId, L2Genesis]
) {

    /** This state's [[L2StateHash]] — the value the head certifies on the effects a partition
      * produces (`docs/spec/l2-state-certificate.md`).
      *
      * ```
      * l2StateHash(S) = blake2b_256(
      *      "gummiworm-l2-state-cardano-eutxo-v1"
      *   || uint32_be(|activeUtxos|)
      *   || for each (input, output) ascending by CBOR(input):
      *          framed(CBOR(input)) || framed(CBOR(output))
      *   || uint32_be(|transientTokens|)
      *   || for each (input, bundle) ascending by CBOR(input):
      *          framed(CBOR(input)) || framed(CBOR(bundle))
      *   || uint32_be(|pendingDeposits|)
      *   || for each (requestId, genesis) ascending by requestId:
      *          framed(int64_be(requestId)) || framed(CBOR(genesis))
      * )
      * ```
      *
      * where `framed(b) = uint32_be(len(b)) || b`.
      *
      * **`commandNumber` is not folded in.** It is a coordination index, not ledger state, and the
      * carriers already name the boundary this digest is taken at (`versionMajor` on the treasury
      * datum, `(versionMajor, versionMinor)` on the SEC). Leaving it out keeps the property the
      * certificate is for: the same state digests the same, whatever route reached it.
      *
      * **Each compartment is counted and each field length-framed**, following
      * [[hydrozoa.multisig.ledger.joint.EvacuationMap.digest]]: none of the encodings is
      * fixed-width, so an unframed concatenation could be read three ways. The counts also keep an
      * empty compartment a defined value rather than a gap.
      *
      * **Order is ascending by the encoded key bytes, compared unsigned byte by byte, shorter
      * prefix first**, so it does not depend on a `Map`'s iteration order. `pendingDeposits` sorts
      * by its own key instead — [[RequestId]]'s packed i64 is fixed-width, so its numeric order and
      * its byte order agree.
      *
      * **The CBOR is re-encoded, not kept raw.** This digest never crosses a backend boundary — it
      * is compared only against peers running this same ledger over the same commands — so unlike
      * the evacuation map's, which is pinned to a Rust counterpart's bytes, it needs determinism
      * across peers rather than fidelity to received bytes, and scalus's encoders give that.
      */
    def stateHash: L2StateHash = {
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

        def cbor[A: io.bullet.borer.Encoder](a: A): Array[Byte] = Cbor.encode(a).toByteArray

        def int64Be(n: Long): Array[Byte] =
            Array.tabulate(8)(i => ((n >>> ((7 - i) * 8)) & 0xffL).toByte)

        // Sort on the encoded key so the order is the encoding's, not a `TransactionInput`
        // `Ordering`'s — the digest is defined over bytes.
        def byEncodedKey[V](entries: Iterable[(TransactionInput, V)]): Seq[(Array[Byte], V)] =
            entries.toSeq.map((i, v) => cbor(i) -> v).sortBy(_._1)(using unsignedBytes)

        buffer.write(L2Snapshot.domainTag)

        val utxos = byEncodedKey(activeUtxos)
        putLength(utxos.size)
        utxos.foreach { (key, output) =>
            putFramed(key)
            putFramed(cbor[TransactionOutput](output))
        }

        val overlay = byEncodedKey(transientTokens)
        putLength(overlay.size)
        overlay.foreach { (key, bundle) =>
            putFramed(key)
            putFramed(cbor[MultiAsset](bundle))
        }

        val deposits = pendingDeposits.toSeq.sortBy(_._1)
        putLength(deposits.size)
        deposits.foreach { (requestId, genesis) =>
            putFramed(int64Be(requestId.asI64))
            putFramed(cbor[L2Genesis](genesis))
        }

        L2StateHash(platform.blake2b_256(ByteString.unsafeFromArray(buffer.toByteArray)))
    }

    /** Unsigned byte-by-byte, shorter prefix first — the same order the evacuation map digest folds
      * in, and the one a byte-slice `Ord` gives in any language.
      */
    private def unsignedBytes: Ordering[Array[Byte]] = (x, y) => {
        val n = math.min(x.length, y.length)
        var i = 0
        var cmp = 0
        while cmp == 0 && i < n do
            cmp = java.lang.Integer.compare(x(i) & 0xff, y(i) & 0xff)
            i += 1
        if cmp != 0 then cmp else java.lang.Integer.compare(x.length, y.length)
    }
}

object L2Snapshot:

    /** Mixed in before anything else so this digest can never collide with a hash of the same bytes
      * taken for another purpose. ASCII, no terminator — the length framing that follows makes the
      * boundary unambiguous.
      *
      * The tag names the backend, because [[L2StateHash]]'s construction is per-backend, and
      * carries a version so it can move when this ledger's state representation does.
      */
    val domainTag: Array[Byte] = "gummiworm-l2-state-cardano-eutxo-v1".getBytes(UTF_8)

    /** Project a full ledger state down to its persisted subset. */
    def fromState(state: EutxoL2Ledger.State): L2Snapshot =
        L2Snapshot(
          state.commandNumber,
          state.activeUtxos,
          state.transientTokens,
          state.pendingDeposits
        )
