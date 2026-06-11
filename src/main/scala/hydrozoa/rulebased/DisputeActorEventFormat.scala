package hydrozoa.rulebased

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.rulebased.DisputeActorEvent.*

object DisputeActorEventFormat:

    def humanFormat(peerNum: HeadPeerNumber)(e: DisputeActorEvent): LogEvent =
        val ev = LogEvent.From.forPeer("DisputeActor", peerNum)
        import ev.*
        e match
            case CardanoBackendError(err) =>
                warn(
                  "Cardano backend error encountered. This may be due to timeout, utxo contention," +
                      s" rollbacks, or timing skew, but it may also be a genuine error.\n\tError: \n$err"
                )
            case BuildingTx(label)        => info(s"Building $label Tx")
            case SubmittingTxLabel(label) => info(s"Submitting $label Tx")
            case SubmittingTxFamily(family, txId) =>
                info(s"Submitting $family with Id $txId")
            case TxCbor(pretty, cbor) =>
                debug(s"\n\tPretty: $pretty\n\tcbor: $cbor")
            case TxSubmitSuccess(family, txId) =>
                info(s"SUCCESS submitting $family with Id $txId")
            case LookingForCollateral(addr) =>
                debug(s"Looking for collateral utxos at address $addr")
            case CollateralFound => debug("Found collateral utxo")
            case NoCollateralFound(peerNum2) =>
                error(s"Could not find a collateral utxo for peer $peerNum2")
            case Tallying             => info("Tallying...")
            case ParsingTreasury      => debug("parsing RuleBased Treasury")
            case TreasuryIsUnresolved => info("Treasury is Unresolved")
            case TreasuryIsResolved   => info("Treasury is Resolved")
            case TreasuryFound(value) => debug(s"Found treasury utxo with $value")
