package hydrozoa.multisig.ledger.l1.deposits.map

import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.given
import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap.Partition.Compartment
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMapEvent.*
import io.circe.syntax.*

/** Renderers from [[DepositsMapEvent]] to [[LogEvent]]. */
object DepositsMapEventFormat:

    def humanFormat(e: DepositsMapEvent): LogEvent =
        val ev = LogEvent.From(Map.empty, "DepositsMap")
        import ev.*
        e match
            case PartitionStarted(bce, ste, pr) =>
                debug(
                  "[partition]" +
                      s"\n\t blockCreationEndTime: $bce" +
                      s"\n\t settlementTxEndTime: $ste" +
                      s"\n\t pollResults: ${pr.utxos.asJson}"
                )
            case EntryClassified(entry, compartment) =>
                val desc = compartment match
                    case Compartment.Immature         => "immature."
                    case Compartment.Expired          => "expired (rejected)."
                    case Compartment.Eligible         => "eligible"
                    case Compartment.NotInPollResults => "mature, but not in the poll results."
                debug(s"$entry is $desc")
