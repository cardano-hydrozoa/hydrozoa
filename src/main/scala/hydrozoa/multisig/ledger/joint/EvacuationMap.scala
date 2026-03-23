package hydrozoa.multisig.ledger.joint

import cats.implicits.*
import hydrozoa.multisig.ledger.commitment.KzgCommitment
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.joint.EvacuationMap.mkScalar
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.given
import scala.collection.immutable.TreeMap
import scalus.cardano.ledger.*
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.cardano.onchain.plutus.v2.TxOut
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

final case class EvacuationKey private (byteString: ByteString)

object EvacuationKey:
    def apply(bytes: ByteString): Option[EvacuationKey] = Some(new EvacuationKey(bytes))
    // if bytes.length == 32 then Some(new EvacuationKey(bytes)) else None

final case class EvacuationMap(
    evacuationMap: TreeMap[EvacuationKey, Payout.Obligation]
)(using Ordering[EvacuationKey], ToData[EvacuationKey]) {
    val isEmpty: Boolean = evacuationMap.isEmpty
    val nonEmpty: Boolean = evacuationMap.nonEmpty
    val size: Int = evacuationMap.size

    /** The evac map, where we threw away the "KeepRaw"
      */
    // Its a silly name, but we use the term "value" too much
    val cooked: TreeMap[EvacuationKey, TransactionOutput] =
        evacuationMap.map((i, obligation) => (i, obligation.utxo.value))
    val outputs: Iterable[Payout.Obligation] = evacuationMap.values

    /** The outputs of the evac map, where we threw away the "KeepRaw"
      */
    val outputsCooked: Iterable[TransactionOutput] = evacuationMap.values.map(_.utxo.value)

    def appended(
        otherMap: TreeMap[EvacuationKey, Payout.Obligation]
    ): EvacuationMap =
        EvacuationMap(evacuationMap ++ otherMap)

    def removed(keys: Set[EvacuationKey]): EvacuationMap =
        EvacuationMap(evacuationMap -- keys)

    def kzgCommitment: KzgCommitment = KzgCommitment.calculateKzgCommitment(scalars)

    def scalars: SList[Scalar] = {
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
        evacuationMap.foldLeft(Value.zero)((acc, evacuatee) => acc + evacuatee._2.utxo.value.value)
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

enum EvacuationDiff:
    case Update(key: EvacuationKey, value: Payout.Obligation)
    case Delete(key: EvacuationKey)
