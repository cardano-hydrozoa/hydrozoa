package hydrozoa.rulebased

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.rulebased.EvacuationActorEvent.*

object EvacuationActorEventFormat:

    private def routingKey(peerNum: HeadPeerNumber): String = s"EvacuationActor.$peerNum"

    private def baseCtx(peerNum: HeadPeerNumber): Map[String, String] =
        Map("peer" -> peerNum.toString)

    def humanFormat(peerNum: HeadPeerNumber)(e: EvacuationActorEvent): LogEvent =
        val ev = LogEvent.From(baseCtx(peerNum), routingKey(peerNum))
        import ev.*
        e match
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
