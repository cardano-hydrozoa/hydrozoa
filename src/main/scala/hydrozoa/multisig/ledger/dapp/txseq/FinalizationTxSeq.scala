package hydrozoa.multisig.ledger.dapp.txseq

import hydrozoa.config.head.HeadConfig
import hydrozoa.multisig.ledger.dapp
import hydrozoa.multisig.ledger.dapp.tx
import hydrozoa.multisig.ledger.dapp.tx.*
import scalus.cardano.txbuilder.SomeBuildError

enum FinalizationTxSeq {
    def finalizationTx: FinalizationTx

    /** Merged deinit, optional direct payouts */
    case Monolithic(
        override val finalizationTx: FinalizationTx.Monolithic
    )

    /** Separate deinit, optional direct payouts */
    case WithDeinit(
        override val finalizationTx: FinalizationTx.WithDeinit,
        deinitTx: DeinitTx
    )

    /** Merged deinit, optional direct payouts, and rollout */
    case WithRollouts(
        override val finalizationTx: FinalizationTx.WithRolloutsMerged,
        rolloutTxSeq: RolloutTxSeq
    )

    /** Separate deinit, optional direct payouts, and rollout */
    case WithDeinitAndRollouts(
        override val finalizationTx: FinalizationTx.WithRollouts,
        deinitTx: DeinitTx,
        rolloutTxSeq: RolloutTxSeq
    )
}

object FinalizationTxSeq {
    type Config = HeadConfig.Section

    extension (finalizationTxSeq: FinalizationTxSeq)

        def mbRollouts: List[RolloutTx] = finalizationTxSeq match {
            case FinalizationTxSeq.WithRollouts(_, rolloutTxSeq) => rolloutTxSeq.mbRollouts
            case FinalizationTxSeq.WithDeinitAndRollouts(_, _, rolloutTxSeq) =>
                rolloutTxSeq.mbRollouts
            case _ => List.empty
        }

        def mbDeinit: Option[DeinitTx] = finalizationTxSeq match {
            case FinalizationTxSeq.WithDeinit(finalizationTx, deinitTx)  => Some(deinitTx)
            case FinalizationTxSeq.WithDeinitAndRollouts(_, deinitTx, _) => Some(deinitTx)
            case _                                                       => None
        }

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
