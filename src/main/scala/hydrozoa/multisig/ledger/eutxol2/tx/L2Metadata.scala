package hydrozoa.multisig.ledger.eutxol2.tx

import cats.syntax.all.*
import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.config.head.initialization.InitializationParameters.HeadId.toHex
import hydrozoa.multisig.ledger.l1.token.CIP67
import scala.util.Try
import scalus.cardano.ledger.AuxiliaryData.Metadata as MD
import scalus.cardano.ledger.{AssetName, AuxiliaryData, Metadatum, MultiAsset, Transaction, Word64}

/** The head-label metadata every EUTXO L2 transaction carries. It reuses the L1 transaction
  * metadata layout ([[hydrozoa.multisig.ledger.l1.tx.Metadata]]): the CIP-67 `HYDR` tag (4937)
  * points at a role map, keyed by the transaction role, pointing at a head-id map, pointing at the
  * role's fields.
  *
  * {{{
  * { 4937: { "L2": { <headId hex>: {
  *     "l1BoundOutputs":    List(Int),               // output indices leaving for L1 (withdrawals)
  *     "l2TransientTokens": Map(Int -> <bundle>)     // transient token content per output index
  * } } } }
  * }}}
  *
  * Outputs whose index is absent from `l1BoundOutputs` stay on L2 — the split is expressed by
  * listing only the L1-bound indices rather than tagging every output. `l2TransientTokens` is
  * encoded by [[TransientOutputs]] and omitted entirely when empty.
  *
  * @param l1BoundOutputs
  *   the indices of outputs bound for L1 (withdrawals); all other outputs stay on L2.
  * @param l2TransientTokens
  *   per-output-index transient token content; an index absent from the map carries none.
  */
final case class L2Metadata(
    l1BoundOutputs: List[Int],
    l2TransientTokens: Map[Int, MultiAsset]
)

object L2Metadata {

    /** The transaction role under the CIP-67 head tag (the L1 layout's transaction-type slot). */
    val role: String = "L2"

    private val l1BoundOutputsKey: Metadatum = Metadatum.Text("l1BoundOutputs")
    private val l2TransientTokensKey: Metadatum = Metadatum.Text("l2TransientTokens")

    /** Build the auxiliary data pinning `headId` and carrying `metadata`. */
    def asAuxData(headId: HeadId, metadata: L2Metadata): AuxiliaryData = {
        val l1BoundList: Metadatum =
            Metadatum.List(
              metadata.l1BoundOutputs.sorted.map(i => Metadatum.Int(i.toLong)).toIndexedSeq
            )
        val fields: Map[Metadatum, Metadatum] =
            Map(l1BoundOutputsKey -> l1BoundList) ++ (
              if metadata.l2TransientTokens.isEmpty then Map.empty
              else
                  Map(
                    l2TransientTokensKey -> TransientOutputs.encodeMetadatum(
                      metadata.l2TransientTokens
                    )
                  )
            )
        val headMap = Metadatum.Map(Map(Metadatum.Text(headId.toHex) -> Metadatum.Map(fields)))
        val roleMap = Metadatum.Map(Map(Metadatum.Text(role) -> headMap))
        MD(Map(Word64(CIP67.Tags.head) -> roleMap))
    }

    /** Parse the head-label metadata out of `tx`, returning the pinned headId and the L2 metadata.
      * Rejects a missing/malformed tag, a role other than `"L2"`, more than one headId, and a
      * missing or ill-shaped `l1BoundOutputs`/`l2TransientTokens`.
      */
    def parse(tx: Transaction): Either[String, (HeadId, L2Metadata)] =
        for {
            md <- tx.auxiliaryData match {
                case Some(keepRaw) =>
                    keepRaw.value match {
                        case m: MD => Right(m)
                        case _     => Left("L2 metadata: auxiliary data is not metadata")
                    }
                case None => Left("L2 metadata: transaction carries no auxiliary data")
            }
            roleMap <- requireMap(
              md.metadata.get(Word64(CIP67.Tags.head)),
              s"HYDR tag ${CIP67.Tags.head}"
            )
            headMap <- requireMap(roleMap.entries.get(Metadatum.Text(role)), s"role '$role'")
            headEntry <- headMap.entries.toList match {
                case entry :: Nil => Right(entry)
                case entries =>
                    Left(s"L2 metadata: expected exactly one headId, got ${entries.length}")
            }
            (headIdRaw, fieldsRaw) = headEntry
            headId <- headIdRaw match {
                case Metadatum.Text(h) =>
                    Try(HeadId(AssetName.fromHex(h))).toEither.left
                        .map(e => s"L2 metadata: malformed headId '$h' ($e)")
                case other => Left(s"L2 metadata: headId key must be Text, got $other")
            }
            fields <- fieldsRaw match {
                case m: Metadatum.Map => Right(m)
                case other => Left(s"L2 metadata: headId payload must be a Map, got $other")
            }
            l1BoundOutputs <- fields.entries.get(l1BoundOutputsKey) match {
                case Some(Metadatum.List(items)) =>
                    items.toList.traverse {
                        case Metadatum.Int(i) if i >= 0 && i <= Int.MaxValue => Right(i.toInt)
                        case other =>
                            Left(
                              s"L2 metadata: l1BoundOutputs index must be a non-negative Int, got $other"
                            )
                    }
                case Some(other) => Left(s"L2 metadata: l1BoundOutputs must be a List, got $other")
                case None        => Left("L2 metadata: l1BoundOutputs field missing")
            }
            l2TransientTokens <- fields.entries.get(l2TransientTokensKey) match {
                case Some(metadatum) => TransientOutputs.decodeMetadatum(metadatum)
                case None            => Right(Map.empty[Int, MultiAsset])
            }
        } yield (headId, L2Metadata(l1BoundOutputs, l2TransientTokens))

    private def requireMap(entry: Option[Metadatum], what: String): Either[String, Metadatum.Map] =
        entry match {
            case Some(m: Metadatum.Map) => Right(m)
            case Some(other)            => Left(s"L2 metadata: $what must be a Map, got $other")
            case None                   => Left(s"L2 metadata: $what not found")
        }
}
