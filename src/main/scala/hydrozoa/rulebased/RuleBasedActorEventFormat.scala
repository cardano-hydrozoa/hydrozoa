package hydrozoa.rulebased

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.rulebased.RuleBasedActorEvent.*

object RuleBasedActorEventFormat:

    def humanFormat(peerNum: HeadPeerNumber)(e: RuleBasedActorEvent): LogEvent =
        val ev = LogEvent.From.forPeer("RuleBasedActor", peerNum)
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
            case NoCollateralFound(peerLabel) =>
                error(s"Could not find a collateral utxo for peer $peerLabel")
            case Tallying             => info("Tallying...")
            case ParsingTreasury      => debug("parsing RuleBased Treasury")
            case TreasuryIsUnresolved => info("Treasury is Unresolved")
            case TreasuryIsResolved   => info("Treasury is Resolved")
            case TreasuryFound(value) => debug(s"Found treasury utxo with $value")

            case BackendErrorContinuingTxs(err) =>
                warn(s"Backend error querying continuing txs. Will retry.\n\tError: $err")
            case BackendErrorTreasuryUtxos(err) =>
                warn(s"Backend error querying treasury UTxOs. Will retry.\n\tError: $err")
            case BackendErrorFeeUtxos(err) =>
                warn(s"Backend error querying fee UTxOs. Will retry.\n\tError: $err")
            case BackendErrorSubmittingEvacTx(err) =>
                warn(s"Backend error submitting evacuation tx. Will retry.\n\tError: $err")
            case TreasuryNotYetResolved =>
                debug("Treasury not yet resolved, retrying")
            case NoMoreEvacuations =>
                info("No more evacuations to be done. Staying alive in case of rollbacks")
            case PayoutObligationsLeft(n) =>
                info(s"$n payout obligations left")
            case NoFeeCollateralUtxo =>
                debug("No fee/collateral UTxO found at wallet address, retrying")
            case BuildingEvacTx(treasuryValue, evacuateeCount, totalValue) =>
                debug(
                  s"Building EvacuationTx with:\n treasury value: $treasuryValue\n # evacuatees:" +
                      s" $evacuateeCount\n total evacuation value $totalValue"
                )
            case SubmittingEvacTx(evacuatedOutputs, cbor) =>
                debug(
                  s"submitting evacTx with $evacuatedOutputs evacuated outputs\n cbor:\n\n$cbor\n\n"
                )
            case EvacTxSubmitted =>
                info("Evacuation tx submitted")
