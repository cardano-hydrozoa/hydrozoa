package hydrozoa.multisig.backend.cardano

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.backend.cardano.CardanoBackendEvent.*

object CardanoBackendEventFormat:

    def humanFormat(e: CardanoBackendEvent): LogEvent =
        val ev = LogEvent.From(Map.empty, "CardanoBackend")
        import ev.*
        e match
            case MockStarted =>
                info("Running Cardano backend mock in IO...")
            case MockInitialUtxos(ids) =>
                debug(s"initial utxo ids: $ids")
            case IsTxKnownChecked(txHash, knownTxs) =>
                trace(s"isTxKnown: looking for tx $txHash in state.knownTxs $knownTxs")
            case SubmitTxReceived(txId) =>
                debug(s"submitTx: $txId")
            case SubmitTxCbor(cbor) =>
                trace(s"submitTx cbor: $cbor")
            case SubmitTxUtxoSetSize(count) =>
                trace(s"utxos count: $count")
            case SubmitTxMissingInputs(missing) =>
                trace(s"missing utxos: $missing")
            case SubmitTxRelevantUtxos(utxos) =>
                trace(s"tx utxos: $utxos")
            case SubmitTxAlreadyKnown(txId) =>
                debug(s"tx $txId is already known, do nothing")
            case FetchScriptFailed(utxoRef, error) =>
                warn(
                  s"Failed to fetch reference script for UTXO $utxoRef",
                  "error" -> error.getMessage
                )
