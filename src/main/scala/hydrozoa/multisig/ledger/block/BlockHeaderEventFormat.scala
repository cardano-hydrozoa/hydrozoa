package hydrozoa.multisig.ledger.block

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.ledger.block.BlockHeaderEvent.*

/** Renderers from [[BlockHeaderEvent]] to [[LogEvent]]. */
object BlockHeaderEventFormat:

    /** Routes under `BlockHeader`. */
    def humanFormat(e: BlockHeaderEvent): LogEvent = {
        val ev = LogEvent.From(Map.empty, "BlockHeader")
        import ev.*
        e match {
            case NextMinor(fmbt, mddwt) =>
                trace(
                  s"nextHeaderMinor: forcedMajorBlockWakeupTime=$fmbt, " +
                      s"mDepositDecisionWakeupTime=$mddwt"
                )
            case NextMajor(fmbt, mddwt) =>
                trace(
                  s"nextHeaderMajor: forcedMajorBlockWakeupTime=$fmbt, " +
                      s"mDepositDecisionWakeupTime=$mddwt"
                )
        }
    }
