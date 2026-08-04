package hydrozoa.multisig.backend.cardano

import hydrozoa.multisig.ledger.l1.tx.EnrichedTx

/** Events emitted by [[FirewalledCardanoBackend]] — one variant per firewalled channel. Each
  * carries the full [[EnrichedTx]] so consumers can recover its id, family, etc.
  */
sealed trait FirewalledCardanoBackendEvent

object FirewalledCardanoBackendEvent:
    final case class DroppedOutboundTx(tx: EnrichedTx[?]) extends FirewalledCardanoBackendEvent

    /** Pass-through submission — records the underlying backend's result so tests can assert on
      * accept vs reject without touching the mock's internal shape.
      */
    final case class SubmittedTx(
        tx: EnrichedTx[?],
        result: Either[CardanoBackend.Error, Unit],
    ) extends FirewalledCardanoBackendEvent
