package hydrozoa.multisig.ledger.l1.deposits.map

import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, SettlementTxEndTime}
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap.{Entry, Existence, Partition}

sealed trait DepositsMapEvent

object DepositsMapEvent:

    final case class PartitionStarted(
        blockCreationEndTime: BlockCreationEndTime,
        settlementTxEndTime: SettlementTxEndTime,
        existence: Existence
    ) extends DepositsMapEvent

    final case class EntryClassified(
        entry: Entry,
        compartment: Partition.Compartment
    ) extends DepositsMapEvent
