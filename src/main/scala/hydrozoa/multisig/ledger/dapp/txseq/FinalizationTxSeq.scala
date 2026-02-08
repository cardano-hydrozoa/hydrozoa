package hydrozoa.multisig.ledger.dapp.txseq

import hydrozoa.config.head.HeadConfig
import hydrozoa.multisig.ledger.dapp
import hydrozoa.multisig.ledger.dapp.tx
import hydrozoa.multisig.ledger.dapp.tx.*
import scalus.cardano.txbuilder.SomeBuildError

enum FinalizationTxSeq {
    def finalizationTx: FinalizationTx

    /** Merged deinit, optional direct payouts */
    case NoPayouts(
        override val finalizationTx: FinalizationTx.NoPayouts
    )

    /** Merged deinit, optional direct payouts, and rollout */
    case WithRollouts(
        override val finalizationTx: FinalizationTx.WithRollouts,
        rolloutTxSeq: RolloutTxSeq
    )
}

object FinalizationTxSeq {
    type Config = HeadConfig.Section

    extension (finalizationTxSeq: FinalizationTxSeq)

        def mbRollouts: List[RolloutTx] = ???

        def mbDeinit: Option[DeinitTx] = ???

    import Builder.*

    final case class Builder(config: Config) {
        def build(args: Any): Either[Builder.Error, FinalizationTxSeq] = ???
    }

    object Builder {

        enum Error:
            case SettlementError(e: (SomeBuildError, String))
            case DeinitTxError(e: (SomeBuildError, String))
            case FinalizationPartialError(e: (SomeBuildError, String))
            case FinalizationCompleteError(e: (SomeBuildError, String))
            case RolloutSeqError(e: (SomeBuildError, String))
    }
}
