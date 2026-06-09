package hydrozoa.rulebased

import hydrozoa.lib.logging.{Level, LogEvent}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.rulebased.DisputeActorEvent.*

object DisputeActorEventFormat:

    private def routingKey(peerNum: HeadPeerNumber): String = s"DisputeActor.$peerNum"

    private def baseCtx(peerNum: HeadPeerNumber): Map[String, String] =
        Map("peer" -> peerNum.toString)

    def humanFormat(peerNum: HeadPeerNumber)(e: DisputeActorEvent): LogEvent =
        val rk = Some(routingKey(peerNum))
        val ctx0 = baseCtx(peerNum)
        e match
            case CardanoBackendError(err) =>
                LogEvent(
                  Level.Warn,
                  "Cardano backend error encountered. This may be due to timeout, utxo contention," +
                      s" rollbacks, or timing skew, but it may also be a genuine error.\n\tError: \n$err",
                  ctx0,
                  routingKey = rk
                )
            case BuildingTx(label) =>
                LogEvent(Level.Info, s"Building $label Tx", ctx0, routingKey = rk)
            case SubmittingTxLabel(label) =>
                LogEvent(Level.Info, s"Submitting $label Tx", ctx0, routingKey = rk)
            case SubmittingTxFamily(family, txId) =>
                LogEvent(Level.Info, s"Submitting $family with Id $txId", ctx0, routingKey = rk)
            case TxCbor(pretty, cbor) =>
                LogEvent(
                  Level.Debug,
                  s"\n\tPretty: $pretty\n\tcbor: $cbor",
                  ctx0,
                  routingKey = rk
                )
            case TxSubmitSuccess(family, txId) =>
                LogEvent(
                  Level.Info,
                  s"SUCCESS submitting $family with Id $txId",
                  ctx0,
                  routingKey = rk
                )
            case LookingForCollateral(addr) =>
                LogEvent(
                  Level.Debug,
                  s"Looking for collateral utxos at address $addr",
                  ctx0,
                  routingKey = rk
                )
            case CollateralFound =>
                LogEvent(Level.Debug, "Found collateral utxo", ctx0, routingKey = rk)
            case NoCollateralFound(peerNum2) =>
                LogEvent(
                  Level.Error,
                  s"Could not find a collateral utxo for peer $peerNum2",
                  ctx0,
                  routingKey = rk
                )
            case Tallying =>
                LogEvent(Level.Info, "Tallying...", ctx0, routingKey = rk)
            case ParsingTreasury =>
                LogEvent(Level.Debug, "parsing RuleBased Treasury", ctx0, routingKey = rk)
            case TreasuryIsUnresolved =>
                LogEvent(Level.Info, "Treasury is Unresolved", ctx0, routingKey = rk)
            case TreasuryIsResolved =>
                LogEvent(Level.Info, "Treasury is Resolved", ctx0, routingKey = rk)
            case TreasuryFound(value) =>
                LogEvent(Level.Debug, s"Found treasury utxo with $value", ctx0, routingKey = rk)
