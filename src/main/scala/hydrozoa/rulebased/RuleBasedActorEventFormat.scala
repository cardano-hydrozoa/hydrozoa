package hydrozoa.rulebased

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.rulebased.RuleBasedActorEvent.*

object RuleBasedActorEventFormat:

    def humanFormat(peerNum: HeadPeerNumber)(e: RuleBasedActorEvent): LogEvent =
        val ev = LogEvent.From.forPeer("RuleBasedActor", peerNum)
        import ev.*
        e match
            case Backend.Error(err) =>
                warn(
                  "Cardano backend error encountered. This may be due to timeout, utxo contention," +
                      s" rollbacks, or timing skew, but it may also be a genuine error.\n\tError: \n$err"
                )
            case Backend.ErrorContinuingTxs(err) =>
                warn(s"Backend error querying continuing txs. Will retry.\n\tError: $err")
            case Backend.ErrorTreasuryUtxos(err) =>
                warn(s"Backend error querying treasury UTxOs. Will retry.\n\tError: $err")
            case Backend.ErrorFeeUtxos(err) =>
                warn(s"Backend error querying fee UTxOs. Will retry.\n\tError: $err")
            case Backend.ErrorSubmittingEvacTx(err) =>
                warn(s"Backend error submitting evacuation tx. Will retry.\n\tError: $err")

            case Treasury.Parsing        => debug("parsing RuleBased Treasury")
            case Treasury.Unresolved     => info("Treasury is Unresolved")
            case Treasury.Resolved       => info("Treasury is Resolved")
            case Treasury.Found(value)   => debug(s"Found treasury utxo with $value")
            case Treasury.NotYetResolved => debug("Treasury not yet resolved, retrying")

            case Collateral.Looking(addr) =>
                debug(s"Looking for collateral utxos at address $addr")
            case Collateral.Found => debug("Found collateral utxo")
            case Collateral.NotFound(peerLabel) =>
                error(s"Could not find a collateral utxo for peer $peerLabel")
            case Collateral.NoFeeCollateralUtxo =>
                debug("No fee/collateral UTxO found at wallet address, retrying")

            case Tx.Building(family) => info(s"Building $family")
            case Tx.Submitting(tx) =>
                info(s"Submitting ${tx.transactionFamily} with Id ${tx.tx.id}")
            case Tx.SubmitSuccess(tx) =>
                info(s"SUCCESS submitting ${tx.transactionFamily} with Id ${tx.tx.id}")
            case Tx.Tallying => info("Tallying...")

            case Evacuation.NoMore =>
                info("No more evacuations to be done. Staying alive in case of rollbacks")
            case Evacuation.PayoutsLeft(n) =>
                info(s"$n payout obligations left")
