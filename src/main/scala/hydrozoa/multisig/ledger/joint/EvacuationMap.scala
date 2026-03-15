package hydrozoa.multisig.ledger.joint

import cats.implicits.*
import hydrozoa.multisig.ledger.commitment.KzgCommitment
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import scala.collection.immutable.TreeMap
import scalus.cardano.ledger.*
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.uplc.builtin.Builtins.{blake2b_224, serialiseData}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data, ToData}
import scalus.|>
import supranational.blst.Scalar

given toDataTransactionInput: ToData[TransactionInput] with {
    override def apply(i: TransactionInput): Data =
        toData(LedgerToPlutusTranslation.getTxOutRefV3(i))
}

given evacuationKeyOrdering: Ordering[EvacuationKey] with {
    override def compare(x: EvacuationKey, y: EvacuationKey): Int =
        summon[Ordering[ByteString]].compare(x.byteString, y.byteString)
}

given evacuationKeyToData: ToData[EvacuationKey] with {
    override def apply(v1: EvacuationKey): Data = toData(v1.byteString)
}

final case class EvacuationKey private (byteString: ByteString)

object EvacuationKey:
    def apply(bytes: ByteString): Option[EvacuationKey] = Some(new EvacuationKey(bytes))
    // if bytes.length == 32 then Some(new EvacuationKey(bytes)) else None

final case class EvacuationMap(
    evacuationMap: TreeMap[EvacuationKey, KeepRaw[TransactionOutput]]
)(using Ordering[EvacuationKey], ToData[EvacuationKey]) {
    val isEmpty: Boolean = evacuationMap.isEmpty
    val nonEmpty: Boolean = evacuationMap.nonEmpty
    val size: Int = evacuationMap.size

    /** The evac map, where we threw away the "KeepRaw".
      */
    // Its a silly name, but we use the term "value" too much
    val cooked: TreeMap[EvacuationKey, TransactionOutput] =
        evacuationMap.map((i, kr) => (i, kr.value))
    val outputs: Iterable[KeepRaw[TransactionOutput]] = evacuationMap.values

    /** The outputs of the evac map, where we threw away the "KeepRaw"
      */
    val outputsCooked: Iterable[TransactionOutput] = evacuationMap.values.map(_.value)

    def appended(
        otherMap: TreeMap[EvacuationKey, KeepRaw[TransactionOutput]]
    ): EvacuationMap =
        EvacuationMap(evacuationMap ++ otherMap)

    def removed(keys: Set[EvacuationKey]): EvacuationMap =
        EvacuationMap(evacuationMap -- keys)

    def kzgCommitment: KzgCommitment = KzgCommitment.calculateKzgCommitment(scalars)

    private def scalars: SList[Scalar] = {
        SList.from(
          evacuationMap.toList.map(e =>
              // FIXME: redundant CBOR encoding with `Sized`, since we're keeping the original serialization anyways
              (e._1, LedgerToPlutusTranslation.getTxOutV2(Sized(e._2.value)))
                  |> ToData.tupleToData
                  |> serialiseData
                  |> blake2b_224
                  |> (_.bytes)
                  |> Scalar().from_bendian
          )
        )
    }
}

object EvacuationMap:
    def empty: EvacuationMap = EvacuationMap(TreeMap.empty)

    def applyDiffs(evacuationMap: EvacuationMap, diffs: Seq[EvacuationDiff]): EvacuationMap =
        evacuationMap |> diffs
            .map {
                case EvacuationDiff.Update(key, value) =>
                    (em: EvacuationMap) => EvacuationMap(em.evacuationMap.updated(key, value))
                case EvacuationDiff.Delete(key) =>
                    (em: EvacuationMap) => EvacuationMap(em.evacuationMap.removed(key))
            }
            .foldLeft(identity: EvacuationMap => EvacuationMap)(_.andThen(_))

// TODO: Turn into a Map (for Add) and a Set (for delete)
enum EvacuationDiff:
    case Update(key: EvacuationKey, value: KeepRaw[TransactionOutput])
    case Delete(key: EvacuationKey)
