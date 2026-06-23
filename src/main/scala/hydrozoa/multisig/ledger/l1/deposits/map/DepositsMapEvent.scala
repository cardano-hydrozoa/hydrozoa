package hydrozoa.multisig.ledger.l1.deposits.map

import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, SettlementTxEndTime}
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap.{Entry, Partition}

sealed trait DepositsMapEvent

object DepositsMapEvent:

    final case class PartitionStarted(
        blockCreationEndTime: BlockCreationEndTime,
        settlementTxEndTime: SettlementTxEndTime,
        pollResults: PollResults
    ) extends DepositsMapEvent

    final case class EntryClassified(
        entry: Entry,
        compartment: Partition.Compartment
    ) extends DepositsMapEvent
