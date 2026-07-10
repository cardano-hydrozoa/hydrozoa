package hydrozoa.multisig.backend.cardano

import scalus.cardano.ledger.TransactionHash

/** Events emitted by [[FirewalledCardanoBackend]] — one variant per firewalled channel. */
sealed trait FirewalledCardanoBackendEvent

object FirewalledCardanoBackendEvent:
    final case class DroppedOutboundTx(txHash: TransactionHash)
        extends FirewalledCardanoBackendEvent

    /** Pass-through submission — records the underlying backend's result so tests can assert on
      * accept vs reject without touching the mock's internal shape.
      */
    final case class SubmittedTx(
        txHash: TransactionHash,
        result: Either[CardanoBackend.Error, Unit],
    ) extends FirewalledCardanoBackendEvent
