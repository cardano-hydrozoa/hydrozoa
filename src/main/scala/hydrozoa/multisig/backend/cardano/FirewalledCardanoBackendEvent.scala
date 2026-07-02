package hydrozoa.multisig.backend.cardano

import scalus.cardano.ledger.TransactionHash

/** Events emitted by [[FirewalledCardanoBackend]] — one variant per firewalled channel. */
sealed trait FirewalledCardanoBackendEvent

object FirewalledCardanoBackendEvent:
    final case class DroppedOutboundTx(txHash: TransactionHash)
        extends FirewalledCardanoBackendEvent
