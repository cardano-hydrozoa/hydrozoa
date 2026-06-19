package hydrozoa.multisig.backend.cardano

import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxos}

sealed trait CardanoBackendEvent

object CardanoBackendEvent:

    // ── Mock-specific events ─────────────────────────────────────────────────

    case object MockStarted extends CardanoBackendEvent
    final case class MockInitialUtxos(utxoIds: Iterable[TransactionInput])
        extends CardanoBackendEvent
    final case class IsTxKnownChecked(txHash: TransactionHash, knownTxs: Set[TransactionHash])
        extends CardanoBackendEvent
    final case class SubmitTxReceived(txId: TransactionHash) extends CardanoBackendEvent
    final case class SubmitTxCbor(cbor: String) extends CardanoBackendEvent
    final case class SubmitTxUtxoSetSize(count: Int) extends CardanoBackendEvent
    final case class SubmitTxMissingInputs(missing: Set[TransactionInput])
        extends CardanoBackendEvent
    final case class SubmitTxRelevantUtxos(utxos: Utxos) extends CardanoBackendEvent
    final case class SubmitTxAlreadyKnown(txId: TransactionHash) extends CardanoBackendEvent

    // ── Blockfrost-specific events ───────────────────────────────────────────

    final case class FetchScriptFailed(utxoRef: TransactionInput, error: CardanoBackend.Error)
        extends CardanoBackendEvent
