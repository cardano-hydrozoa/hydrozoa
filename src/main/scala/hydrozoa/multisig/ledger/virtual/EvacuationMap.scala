package hydrozoa.multisig.ledger.virtual

import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import scala.collection.immutable.TreeMap
import scalus.cardano.ledger.*
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.uplc.builtin.Builtins.{blake2b_224, serialiseData}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{Data, ToData}
import scalus.|>
import supranational.blst.Scalar

given toDataTransactionInput: ToData[TransactionInput] with {
    override def apply(i: TransactionInput): Data =
        toData(LedgerToPlutusTranslation.getTxOutRefV3(i))
}

final case class EvacuationMap[EvacuationKey](
    evacMap: TreeMap[EvacuationKey, KeepRaw[TransactionOutput]]
)(using Ordering[EvacuationKey], ToData[EvacuationKey]) {
    val isEmpty: Boolean = evacMap.isEmpty
    val nonEmpty: Boolean = evacMap.nonEmpty
    val size: Int = evacMap.size

    /** The evac map, where we threw away the "KeepRaw".
      */
    // Its a silly name, but we use the term "value" too much
    val cooked: TreeMap[EvacuationKey, TransactionOutput] = evacMap.map((i, kr) => (i, kr.value))
    val outputs: Iterable[KeepRaw[TransactionOutput]] = evacMap.values

    /** The outputs of the evac map, where we threw away the "KeepRaw"
      */
    val outputsCooked: Iterable[TransactionOutput] = evacMap.values.map(_.value)

    def appended(
        otherMap: TreeMap[EvacuationKey, KeepRaw[TransactionOutput]]
    ): EvacuationMap[EvacuationKey] =
        EvacuationMap(evacMap ++ otherMap)

    def removed(keys: Set[EvacuationKey]): EvacuationMap[EvacuationKey] =
        EvacuationMap(evacMap -- keys)

    def kzgCommitment: KzgCommitment = KzgCommitment.calculateKzgCommitment(scalars)

    private def scalars: SList[Scalar] = {
        SList.from(
          evacMap.toList.map(e =>
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
    def empty[EvacuationKey](using
        Ordering[EvacuationKey],
        ToData[EvacuationKey]
    ): EvacuationMap[EvacuationKey] = EvacuationMap(TreeMap.empty)
