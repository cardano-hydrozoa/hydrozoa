package hydrozoa.multisig.ledger.l1.deposits.map

import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, SettlementTxEndTime}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.DepositAbsorptionStartTime
import hydrozoa.config.head.multisig.timing.{TxTiming, given_Ordering_DepositAbsorptionStartTime}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap.Partition.Compartment
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap.{Entry, Partition}
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

    lazy val numberOfDeposits: Int = treeMap.values.map(_.size).sum

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
      *   - NotInPollResults -- mature, but not eligible for absorption because the deposit is not on chain
      *   - - expire -- deposits absorption window is expire before this settlement tx's TTL is reached
      */
    def partition[F[_]: Monad](tracer: ContraTracer[F, DepositsMapEvent])(
        blockCreationEndTime: BlockCreationEndTime,
        settlementTxEndTime: SettlementTxEndTime,
        existence: DepositsMap.Existence
    ): F[DepositsMap.Partition] =
        for {
            _ <- tracer.traceWith(
              DepositsMapEvent.PartitionStarted(
                blockCreationEndTime,
                settlementTxEndTime,
                existence
              )
            )
            result <- treeMap.toList.foldM(Partition.empty) { case (outerAcc, (_, depositQueue)) =>
                depositQueue.toList.foldM(outerAcc) { case (innerAcc, entry) =>
                    import Compartment.*
                    val isImmature =
                        TxTiming.depositIsImmature(
                          entry.depositUtxo.absorptionStartTime,
                          blockCreationEndTime
                        )
                    val isExpired =
                        TxTiming.depositIsExpired(
                          settlementTxEndTime,
                          entry.depositUtxo.absorptionEndTime
                        )
                    val isExistent = existence.isExistent(entry)
                    // A deposit whose absorption window closes before the settlement tx's
                    // validity ends can never be safely absorbed, so it must be rejected even
                    // if it has not matured yet — `isExpired` takes precedence over `isImmature`.
                    val compartment =
                        if isExpired then Expired
                        else if isImmature then Immature
                        else if isExistent then Eligible
                        else NotInPollResults
                    tracer
                        .traceWith(DepositsMapEvent.EntryClassified(entry, compartment))
                        .as(innerAcc.append(compartment, entry))
                }
            }
        } yield result
}

object DepositsMap {
    def empty: DepositsMap = DepositsMap(
      TreeMap.empty[DepositAbsorptionStartTime, Queue[Entry]]
    )

    final case class Entry(
        requestId: RequestId,
        depositUtxo: DepositUtxo
    )

    /** How [[DepositsMap.partition]] decides whether a mature, unexpired deposit still exists on L1
      * — the only classification input that differs between peer roles. All other checks (immature
      * / expired / the absorption cap) are role-independent.
      *
      *   - [[FromPoll]] — the peer's own fresh L1 poll. The head-peer path: head peers hold
      *     settlement keys, so a settlement can never spend an absorbed deposit without their
      *     signature; their fresh poll therefore always reflects the pre-settlement L1 and they
      *     verify existence independently.
      *   - [[FromLeaderView]] — replay the soft-confirmed leader's verdict carried by the block
      *     brief. The coil-peer path: a coil below the settlement quorum can lag behind an
      *     already-submitted settlement that has spent the absorbed deposits, so a fresh poll would
      *     spuriously report them gone. Instead the coil trusts the head peers' unanimous view — a
      *     deposit is existent iff the leader did **not** reject it (`depositsRejected`). This
      *     handles every compartment: absorbed and eligible-but-cap-unabsorbed deposits are both
      *     absent from `depositsRejected` (existent), while leader-rejected ones are present
      *     (non-existent); immature/expired deposits never reach this check.
      */
    enum Existence {
        case FromPoll(pollResults: PollResults)
        case FromLeaderView(rejectedRequestIds: Set[RequestId])

        def isExistent(entry: Entry): Boolean = this match {
            case FromPoll(pollResults) =>
                pollResults.utxos.contains(entry.depositUtxo.toUtxo.input)
            case FromLeaderView(rejectedRequestIds) =>
                !rejectedRequestIds.contains(entry.requestId)
        }
    }

    final case class Partition private[map] (
        expired: DepositsMap,
        eligible: DepositsMap,
        immature: DepositsMap,
        notInPollResults: DepositsMap
    ) {
        def append(compartment: Compartment, x: Entry): Partition =
            compartment match {
                case Compartment.Expired => copy(expired = expired.append(x))
                case Compartment.NotInPollResults =>
                    copy(notInPollResults = notInPollResults.append(x))
                case Compartment.Eligible => copy(eligible = eligible.append(x))
                case Compartment.Immature => copy(immature = immature.append(x))
            }

        /** Take the first `n` eligible '''deposits''' for absorption, in absorption-start order and
          * then queue order, leaving the rest unabsorbed.
          *
          * `n` counts deposits, not map keys. The map is two-layer — one queue per absorption start
          * time, and that time is slot-quantized, so any deposits maturing in the same second share
          * a key — which means a key-boundary split cannot express the bound at all: a single key
          * may hold more than `n` deposits on its own. The cut therefore falls '''inside''' a queue
          * whenever the budget runs out mid-key. Queue order is the request-stream subsequence, so
          * cutting there absorbs the earliest requests of that second and leaves the later ones for
          * the next block.
          *
          * Bounded by `n`, not by the size of the map: keys are walked in ascending order only
          * until the budget is spent, `sizeIs` stops measuring an oversized queue as soon as it is
          * known not to fit, and the unabsorbed tail is taken as a range rather than traversed.
          */
        def split(n: Int): Split = {
            // The first key whose queue does not fit whole in the remaining budget, with the room
            // left for it; `None` if every eligible deposit fits.
            @annotation.tailrec
            def boundary(
                remaining: Iterator[(DepositAbsorptionStartTime, Queue[Entry])],
                taken: Int
            ): Option[(DepositAbsorptionStartTime, Int)] =
                if !remaining.hasNext then None
                else {
                    val (startTime, queue) = remaining.next()
                    val room = n - taken
                    // `sizeIs` short-circuits: it stops counting once the queue is known to be
                    // longer than `room`, so an enormous same-second queue is never measured.
                    if queue.sizeIs > room then Some((startTime, room))
                    else boundary(remaining, taken + queue.size)
                }

            val (tmAbsorbed, tmUnabsorbed) =
                boundary(eligible.treeMap.iterator, 0) match {
                    case None =>
                        (eligible.treeMap, TreeMap.empty[DepositAbsorptionStartTime, Queue[Entry]])
                    case Some((startTime, room)) =>
                        val (headOfQueue, tailOfQueue) = eligible.treeMap(startTime).splitAt(room)
                        val before = eligible.treeMap.rangeUntil(startTime)
                        val fromHere = eligible.treeMap.rangeFrom(startTime)
                        // `room < queue.size` by construction, so the tail is never empty and the
                        // boundary key always survives; the head is empty only when the budget ran
                        // out exactly on the previous key.
                        (
                          if headOfQueue.isEmpty then before
                          else before.updated(startTime, headOfQueue),
                          fromHere.updated(startTime, tailOfQueue)
                        )
                }
            val absorbed = DepositsMap(tmAbsorbed)
            val unabsorbed = DepositsMap(tmUnabsorbed)
            Split(
              absorbed = absorbed,
              expired = expired,
              notInPollResults = notInPollResults,
              unabsorbed = unabsorbed,
              immature = immature
            )
        }

        override def toString: String =
            "Deposits partitioned:" + "\n\t" +
                s"|- Expired: ${expired.requestIdsLong}" + "\n\t" +
                s"|- Eligible: ${eligible.requestIdsLong}" + "\n\t" +
                s"|- Immature: ${immature.requestIdsLong}" + "\n\t" +
                s"|- NotInPollResults: ${notInPollResults.requestIdsLong}"
    }

    object Partition {
        enum Compartment:
            case Immature, Eligible, Expired, NotInPollResults

        val empty: Partition =
            Partition(DepositsMap.empty, DepositsMap.empty, DepositsMap.empty, DepositsMap.empty)
    }

    final case class Split private[map] (
        absorbed: DepositsMap,
        expired: DepositsMap,
        notInPollResults: DepositsMap,
        unabsorbed: DepositsMap,
        immature: DepositsMap,
    ) {
        lazy val eligible: DepositsMap = absorbed.concat(unabsorbed)
        val surviving: DepositsMap = unabsorbed.concat(immature)
        val decisions: Decisions = Decisions(
          absorbed = absorbed.unzip,
          rejected = DepositsMap(notInPollResults.treeMap ++ expired.treeMap).unzip,
          mNextAbsorptionStartTime = surviving.treeMap.keys.minOption
        )

        override def toString: String =
            "Deposits partitioned and split:" + "\n" +
                "|- " + s"Expired: ${expired.requestIdsLong}" + "\n" +
                "|- " + s"NotInPollResults: ${notInPollResults.requestIdsLong}" + "\n" +
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
