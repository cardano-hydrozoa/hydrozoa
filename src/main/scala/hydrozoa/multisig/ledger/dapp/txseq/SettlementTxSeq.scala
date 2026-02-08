package hydrozoa.multisig.ledger.dapp.txseq

import hydrozoa.config.head.HeadConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.dapp.tx
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import scalus.cardano.txbuilder.SomeBuildError

enum SettlementTxSeq {
    def settlementTx: SettlementTx

    case NoRollouts(
        override val settlementTx: SettlementTx.NoRollouts,
    )

    case WithRollouts(
        override val settlementTx: SettlementTx.WithRollouts,
        rolloutTxSeq: RolloutTxSeq,
    )
}

object SettlementTxSeq {

    type Config = HeadConfig.Section

    extension (settlementTxSeq: SettlementTxSeq)

        def mbRollouts: List[RolloutTx] = settlementTxSeq match {
            case _: NoRollouts   => List.empty
            case r: WithRollouts => r.rolloutTxSeq.mbRollouts
        }

    import Builder.*

    final case class Builder(
        config: SettlementTxSeq.Config
    ) {
        def build(args: Args): Either[Builder.Error, Builder.Result] = ???
    }

    object Builder {
        enum Error:
            case SettlementError(e: (SomeBuildError, String))
            case RolloutSeqError(e: (SomeBuildError, String))
            case FallbackError(e: SomeBuildError)

        final case class Result(
            settlementTxSeq: SettlementTxSeq,
            fallbackTx: FallbackTx,
            override val depositsSpent: Vector[DepositUtxo],
            override val depositsToSpend: Vector[DepositUtxo]
        ) extends DepositUtxo.Many.Spent.Partition

        final case class Args(
            majorVersionProduced: BlockVersion.Major,
            treasuryToSpend: MultisigTreasuryUtxo,
            depositsToSpend: Vector[DepositUtxo],
            payoutObligationsRemaining: Vector[Payout.Obligation],
            kzgCommitment: KzgCommitment,
            competingFallbackValidityStart: QuantizedInstant,
            blockCreatedOn: QuantizedInstant,
        )
        // TODO: confirm: this one is not needed
        // extends SingleArgs(kzgCommitment),
            extends Payout.Obligation.Many.Remaining {}
    }
}
