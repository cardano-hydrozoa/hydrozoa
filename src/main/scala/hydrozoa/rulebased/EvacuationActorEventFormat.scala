package hydrozoa.rulebased

import hydrozoa.lib.logging.{Level, LogEvent}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.rulebased.EvacuationActorEvent.*

object EvacuationActorEventFormat:

    private def routingKey(peerNum: HeadPeerNumber): String = s"EvacuationActor.$peerNum"

    private def baseCtx(peerNum: HeadPeerNumber): Map[String, String] =
        Map("peer" -> peerNum.toString)

    def humanFormat(peerNum: HeadPeerNumber)(e: EvacuationActorEvent): LogEvent =
        val rk = Some(routingKey(peerNum))
        val ctx0 = baseCtx(peerNum)
        e match
            case BackendErrorContinuingTxs(err) =>
                LogEvent(
                  Level.Warn,
                  s"Backend error querying continuing txs. Will retry.\n\tError: $err",
                  ctx0,
                  routingKey = rk
                )
            case BackendErrorTreasuryUtxos(err) =>
                LogEvent(
                  Level.Warn,
                  s"Backend error querying treasury UTxOs. Will retry.\n\tError: $err",
                  ctx0,
                  routingKey = rk
                )
            case BackendErrorFeeUtxos(err) =>
                LogEvent(
                  Level.Warn,
                  s"Backend error querying fee UTxOs. Will retry.\n\tError: $err",
                  ctx0,
                  routingKey = rk
                )
            case BackendErrorSubmittingEvacTx(err) =>
                LogEvent(
                  Level.Warn,
                  s"Backend error submitting evacuation tx. Will retry.\n\tError: $err",
                  ctx0,
                  routingKey = rk
                )
            case TreasuryNotYetResolved =>
                LogEvent(Level.Debug, "Treasury not yet resolved, retrying", ctx0, routingKey = rk)
            case NoMoreEvacuations =>
                LogEvent(
                  Level.Info,
                  "No more evacuations to be done. Staying alive in case of rollbacks",
                  ctx0,
                  routingKey = rk
                )
            case PayoutObligationsLeft(n) =>
                LogEvent(Level.Info, s"$n payout obligations left", ctx0, routingKey = rk)
            case NoFeeCollateralUtxo =>
                LogEvent(
                  Level.Debug,
                  "No fee/collateral UTxO found at wallet address, retrying",
                  ctx0,
                  routingKey = rk
                )
            case BuildingEvacTx(treasuryValue, evacuateeCount, totalValue) =>
                LogEvent(
                  Level.Debug,
                  s"Building EvacuationTx with:\n treasury value: $treasuryValue\n # evacuatees:" +
                      s" $evacuateeCount\n total evacuation value $totalValue",
                  ctx0,
                  routingKey = rk
                )
            case SubmittingEvacTx(evacuatedOutputs, cbor) =>
                LogEvent(
                  Level.Debug,
                  s"submitting evacTx with $evacuatedOutputs evacuated outputs\n cbor:\n\n$cbor\n\n",
                  ctx0,
                  routingKey = rk
                )
            case EvacTxSubmitted =>
                LogEvent(Level.Info, "Evacuation tx submitted", ctx0, routingKey = rk)
