package hydrozoa.integration.stage1

import hydrozoa.config.HeadConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.block.{BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.event.LedgerEvent
import scalus.cardano.ledger.{TransactionInput, TransactionOutput}

/** This should be immutable. */
case class Stage1State(
    headConfig: HeadConfig,
    //
    currentBlock: CurrentBlock,
    currentBlockEvents: List[LedgerEvent],
    //
    currentTime: QuantizedInstant,
    // TODO: move to Done?
    competingFallbackStartTime: QuantizedInstant,
    // L2 state
    activeUtxos: Map[TransactionInput, TransactionOutput]
)

enum CurrentBlock:
    case InProgress(
        blockNumber: BlockNumber,
        creationTime: QuantizedInstant,
        prevVersion: BlockVersion.Full // prev. since the current is not defined until the block will be ended (in the future)
    )
    case Done(
        blockNumber: BlockNumber,
        version: BlockVersion.Full
    )
    case HeadFinalized
