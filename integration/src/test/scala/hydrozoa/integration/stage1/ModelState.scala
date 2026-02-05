package hydrozoa.integration.stage1

import hydrozoa.config.HeadConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.block.{BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.event.LedgerEvent
import scalus.cardano.ledger.{TransactionInput, TransactionOutput}
import test.TestPeer

/** This should be immutable. */
case class ModelState(
    ownTestPeer: TestPeer,

    // Read-only: configuration, initialization
    headConfig: HeadConfig,

    // Block producing cycle
    currentTime: CurrentTime,
    blockCycle: BlockCycle,
    currentBlockEvents: List[LedgerEvent] = List.empty,

    // This is put here to avoid tossing over Done/Ready/InProgress
    // NB: for block zero it's more initializationExpirationTime
    competingFallbackStartTime: QuantizedInstant,

    // L2 state
    activeUtxos: Map[TransactionInput, TransactionOutput],
) {
    override def toString: String = "<model state (hidden)>"
}

enum CurrentTime(qi: QuantizedInstant):
    case BeforeHappyPathExpiration(qi: QuantizedInstant) extends CurrentTime(qi)

    case InSilencePeriod(qi: QuantizedInstant) extends CurrentTime(qi)

    case AfterCompetingFallbackStartTime(qi: QuantizedInstant) extends CurrentTime(qi)

    def instant: QuantizedInstant = qi

enum BlockCycle:

    /** Block is done, delay is ahead.
      */
    case Done(
        blockNumber: BlockNumber,
        version: BlockVersion.Full
    )

    /** Delay is over, ready for a new block. */
    case Ready(
        blockNumber: BlockNumber,
        // We use previous version here since the current version is not defined
        // until the block will be ended (in the future).
        prevVersion: BlockVersion.Full
    )

    /** Block is under construction. */
    case InProgress(
        blockNumber: BlockNumber,
        creationTime: QuantizedInstant,
        // We use previous version here since the current version is not defined
        // until the block will be ended (in the future).
        prevVersion: BlockVersion.Full
    )

    /** The final block is Done */
    case HeadFinalized
