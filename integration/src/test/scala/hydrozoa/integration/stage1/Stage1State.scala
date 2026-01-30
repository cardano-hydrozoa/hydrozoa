package hydrozoa.integration.stage1

import hydrozoa.config.HeadConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.block.BlockNumber
import scalus.cardano.ledger.{TransactionInput, TransactionOutput}

/** This should be immutable. */
case class Stage1State(
    headConfig: HeadConfig,
    currentBlock: CurrentBlock,
    currentTime: QuantizedInstant,
    activeUtxos: Map[TransactionInput, TransactionOutput]
)

enum CurrentBlock:
    case InProgress(blockNumber: BlockNumber)
    case Done(blockNumber: BlockNumber)
    case Finished
