package hydrozoa.multisig.ledger.block

import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.given
import hydrozoa.lib.crypto.Preimage
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import java.nio.charset.StandardCharsets.UTF_8
import scalus.cardano.ledger.Hash32

/** The digest that commits a block to its content, as defined in `design/block-hash.md`.
  *
  * Without it a block's four descriptive layers commit to positions and nothing else: a
  * [[hydrozoa.multisig.ledger.event.RequestId]] names a slot in one peer's sequence, a
  * [[BlockBody]] lists those slots, and a soft-ack signs four header scalars. Two peers holding
  * *different payloads under the same id* therefore agree on every signature. `blockHash` closes
  * that by folding each request's own [[hydrozoa.multisig.consensus.UserRequestBody.hash]] into the
  * block's digest, which the soft-ack then signs.
  *
  * ```
  * blockHash = blake2b_256(
  *      "gummiworm-block-v1"
  *   || u8(blockType)                            -- 0 initial, 1 minor, 2 major, 3 final
  *   -- header
  *   || u32(blockNum) || u32(versionMajor) || u32(versionMinor)
  *   || u64(startTime) || u64(endTime)
  *   || non-final only:
  *        u64(fallbackTxStartTime) || u64(forcedMajorBlockWakeupTime)
  *     || bool(mDepositDecisionWakeupTime.isDefined)
  *     || u64(mDepositDecisionWakeupTime)        -- present only when the flag is true
  *   -- body
  *   || u32(requests.length)
  *   || for each, in list order:
  *        u32(peerNum) || u64(requestNum) || raw(requestHash) || u8(validity)
  *   || u32(depositsAbsorbed.length)
  *   || for each, in list order: u32(peerNum) || u64(requestNum)
  *   || u32(depositsRejected.length)
  *   || for each, in list order: u32(peerNum) || u64(requestNum)
  * )
  * ```
  *
  * Notes on the layout:
  *
  *   - **The block type leads the block's own fields.** The four shapes carry different fields — a
  *     final header has no forward timing, and only a major block absorbs deposits — so the tag is
  *     what keeps them from colliding. Every block type then writes all three body lists, absent
  *     ones as length zero, which is also what makes the digest total on [[BlockBrief.Initial]]'s
  *     empty body.
  *   - **Order is the list's own order**, not sorted. The ordered request list is what the leader
  *     chose and what every follower must reproduce; sorting would hide a reordering, which is a
  *     real disagreement about block content.
  *   - **`RequestNumber` is `u64`.** The `Request` journal key is 8 bytes, unlike the 4-byte
  *     soft/hard-ack indices.
  *   - **The optional wakeup is flag-then-value**, so `None` and a present value can never produce
  *     the same bytes.
  *   - **`ValidityFlag` rides beside each request**, because it is still a [[BlockBody]] field and
  *     the digest covers the brief as it stands. Taking the flags out of the body later changes
  *     this preimage, and therefore [[domainTag]].
  *   - **`blockHash` is excluded from its own preimage.** It is a [[BlockBrief]] field, so every
  *     other field of the brief goes in and this one does not.
  */
object BlockHash {

    /** Mixed in before anything else so this digest can never collide with a hash of the same bytes
      * taken for another purpose. ASCII, no terminator — the block-type tag that follows is
      * fixed-width, so the boundary is unambiguous.
      */
    val domainTag: Array[Byte] = "gummiworm-block-v1".getBytes(UTF_8)

    /** The digest over a block's header and body — every field of a [[BlockBrief]] except the
      * digest itself.
      *
      * It takes the two halves rather than a whole brief so a brief can compute it while it is
      * being built, before there is a brief to pass.
      */
    def apply(header: BlockHeader, body: BlockBody): Hash32 = {
        val out = Preimage()
        out.raw(domainTag)
        out.u8(blockTypeTag(header))

        out.u32(header.blockNum.convert)
        out.u32(header.blockVersion.major.convert)
        out.u32(header.blockVersion.minor.convert)
        out.instant(header.startTime.convert)
        out.instant(header.endTime.convert)
        header match {
            case nonFinal: BlockHeader.NonFinal.Section =>
                out.instant(nonFinal.fallbackTxStartTime.convert)
                out.instant(nonFinal.forcedMajorBlockWakeupTime.convert)
                nonFinal.mDepositDecisionWakeupTime match {
                    case None => out.bool(false)
                    case Some(wakeup) =>
                        out.bool(true)
                        out.instant(wakeup.convert)
                }
            // A final block closes the head: it schedules no fallback, no forced major, and no
            // deposit decision, so its header carries none of those times to commit to.
            case _ => ()
        }

        out.u32(body.requests.size)
        body.requests.foreach { (requestId, requestHash, validity) =>
            putRequestId(out, requestId)
            out.hash32(requestHash)
            out.u8(validityTag(validity))
        }
        out.u32(body.depositsAbsorbed.size)
        body.depositsAbsorbed.foreach(putRequestId(out, _))
        out.u32(body.depositsRejected.size)
        body.depositsRejected.foreach(putRequestId(out, _))

        out.digest
    }

    private def blockTypeTag(blockType: BlockType): Int = blockType match {
        case _: BlockType.Initial => 0x00
        case _: BlockType.Minor   => 0x01
        case _: BlockType.Major   => 0x02
        case _: BlockType.Final   => 0x03
        // `BlockType` cannot be sealed — BlockBrief.scala extends it — so the match needs a
        // fallthrough. Reaching it means a fifth block type was added without a tag, which would
        // silently collide with `Initial`; refuse instead.
        case other =>
            throw IllegalArgumentException(s"block type has no blockHash tag: ${other.getClass}")
    }

    private def validityTag(validity: ValidityFlag): Int = validity match {
        case ValidityFlag.Valid   => 0x00
        case ValidityFlag.Invalid => 0x01
    }

    private def putRequestId(out: Preimage, requestId: RequestId): Unit = {
        out.u32(requestId.peerNum.convert)
        out.u64(requestId.requestNum.convert)
    }
}
