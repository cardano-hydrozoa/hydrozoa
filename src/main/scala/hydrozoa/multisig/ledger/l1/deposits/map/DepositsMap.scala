package hydrozoa.multisig.ledger.l1.deposits.map

import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, SettlementTxEndTime}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.DepositAbsorptionStartTime
import hydrozoa.config.head.multisig.timing.{TxTiming, given_Ordering_DepositAbsorptionStartTime}
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap.Entry
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap.Partition.Compartment
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import scala.collection.immutable.{Queue, TreeMap}

/** deposits in a TreeMap according to their absorption start time. The Tree map ensures that the
  * traversal order is according to the absorption start time, with ties being broken according to
  * the total ordering of the requests, such that each queue in this map is a subsequence of the
  * totally-ordered request stream.
  */
final case class DepositsMap private[map] (
    treeMap: TreeMap[DepositAbsorptionStartTime, Queue[Entry]]
) {

    /** Append a request to the end of the queue of requests with the same start time.
      */
    def append(entry: Entry): DepositsMap = {
        val absorptionStartTime = entry._2.absorptionStartTime
        DepositsMap(treeMap.updatedWith(absorptionStartTime) {
            case None        => Some(Queue(entry))
            case Some(queue) => Some(queue.appended(entry))
        })
    }

    /** Append a queue of requests sharing the same deposit absorption time to the end of the map's
      * corresponding queue.
      */
    def append(entries: (DepositAbsorptionStartTime, Queue[Entry])): DepositsMap =
        DepositsMap(treeMap.updatedWith(entries._1) {
            case None        => Some(entries._2)
            case Some(queue) => Some(queue ++ entries._2)
        })

    def concat(other: DepositsMap): DepositsMap =
        other.treeMap.foldLeft(this)((acc, entries) => acc.append(entries))

    lazy val isEmpty: Boolean = treeMap.isEmpty

    lazy val numberOfDeposits = treeMap.values.map(_.size).sum

    /** Request-deposit tuples traversed in order of absorption start time, with ties broken
      * according to the order in which they were added to the DepositsMap (which should correspond
      * to the total order of the request stream)
      */
    lazy val flatten: Iterable[Entry] = treeMap.values.flatten

    lazy val unzip: DepositsMap.Unzip =
        val (x, y) = treeMap.values.flatten.unzip(using x => (x.requestId, x.depositUtxo))
        DepositsMap.Unzip(x.toList, y.toList)

    lazy val requestIds: List[RequestId] =
        treeMap.values.flatten.iterator.map(_.requestId).toList

    lazy val requestIdsLong: List[Long] =
        requestIds.map(_.asI64)

    lazy val depositUtxos: List[DepositUtxo] =
        treeMap.values.flatten.iterator.map(_.depositUtxo).toList

    /** @return
      *   Queue order:
      *   - eligible for absorption
      *   - ineligible for absorption - (immature + mature but non-existent)
      *   - rejected
      */
    def partition(
        blockCreationEndTime: BlockCreationEndTime,
        settlementTxEndTime: SettlementTxEndTime,
        pollResults: PollResults
    ): DepositsMap.Partition =
        treeMap.foldLeft(DepositsMap.Partition.empty) {
            case (outerAcc, (_absorptionStartTime, depositQueue)) =>
                depositQueue.foldLeft(outerAcc) { case (innerAcc, entry) =>
                    import DepositsMap.Partition.Compartment.*
                    import entry.*

                    val isImmature = TxTiming.depositIsImmature(
                      depositUtxo.absorptionStartTime,
                      blockCreationEndTime
                    )

                    val isExpired = TxTiming.depositIsExpired(
                      settlementTxEndTime,
                      depositUtxo.absorptionEndTime
                    )

                    val isExistent = pollResults.utxos.contains(depositUtxo.toUtxo.input)

                    val compartment =
                        if isImmature then Immature
                        else if isExpired then Rejected
                        else if isExistent then Eligible
                        else Rejected

                    innerAcc.append(compartment, entry)

                }
        }
}

object DepositsMap {
    def empty: DepositsMap = DepositsMap(
      TreeMap.empty[DepositAbsorptionStartTime, Queue[Entry]]
    )

    final case class Entry(
        requestId: RequestId,
        depositUtxo: DepositUtxo
    )

    final case class Partition private[map] (
        rejected: DepositsMap,
        eligible: DepositsMap,
        immature: DepositsMap,
    ) {
        def append(compartment: Compartment, x: Entry): Partition =
            compartment match {
                case Compartment.Rejected => copy(rejected = rejected.append(x))
                case Compartment.Eligible => copy(eligible = eligible.append(x))
                case Compartment.Immature => copy(immature = immature.append(x))
            }

        def split(n: Int): Split = {
            val (tmAbsorbed, tmUnabsorbed) = eligible.treeMap.splitAt(n)
            val absorbed = DepositsMap(tmAbsorbed)
            val unabsorbed = DepositsMap(tmUnabsorbed)
            Split(
              absorbed = absorbed,
              rejected = rejected,
              unabsorbed = unabsorbed,
              immature = immature
            )
        }

        override def toString: String =
            "Deposits partitioned:" + "\n\t" +
                s"|- Rejected: ${rejected.requestIdsLong}" + "\n\t" +
                s"|- Eligible: ${eligible.requestIdsLong}" + "\n\t" +
                s"|- Immature: ${immature.requestIdsLong}"
    }

    object Partition {
        enum Compartment:
            case Immature, Eligible, Rejected

        val empty = Partition(DepositsMap.empty, DepositsMap.empty, DepositsMap.empty)
    }

    final case class Split private[map] (
        absorbed: DepositsMap,
        rejected: DepositsMap,
        unabsorbed: DepositsMap,
        immature: DepositsMap,
    ) {
        lazy val eligible = absorbed.concat(unabsorbed)
        val surviving = unabsorbed.concat(immature)
        val decisions = Decisions(
          absorbed = absorbed.unzip,
          rejected = rejected.unzip,
          mNextAbsorptionStartTime = surviving.treeMap.keys.minOption
        )

        override def toString: String =
            "Deposits partitioned and split:" + "\n" +
                "|- " + s"Rejected: ${rejected.requestIdsLong}" + "\n" +
                "|- " + s"Eligible: ${eligible.requestIdsLong}" + "\n" +
                "|--- " + s"Absorbed: ${absorbed.requestIdsLong}" + "\n" +
                "|--- " + s"Unabsorbed: ${unabsorbed.requestIdsLong}" + "\n" +
                "|- " + s"Surviving: ${surviving.requestIdsLong}" + "\n" +
                "|--- " + s"Unabsorbed: ${unabsorbed.requestIdsLong}" + "\n" +
                "|--- " + s"Immature: ${immature.requestIdsLong}"
    }

    final case class Decisions private[map] (
        absorbed: Unzip,
        rejected: Unzip,
        mNextAbsorptionStartTime: Option[DepositAbsorptionStartTime]
    )

    final case class Unzip private[map] (
        requestIds: List[RequestId],
        depositUtxos: List[DepositUtxo]
    ) {
        def isEmpty: Boolean = requestIds.isEmpty
    }
}
