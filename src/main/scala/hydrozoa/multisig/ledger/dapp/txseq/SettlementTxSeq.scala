package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.State
import cats.implicits.*
import hydrozoa.multisig.ledger.dapp.tx.{FallbackTx, RolloutTx, SettlementTx}
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, RolloutUtxo, TreasuryUtxo}
import scalus.cardano.ledger.{Coin, TransactionInput, TransactionOutput}

object SettlementTxSeq {

    def build(args: Args): Result =
        val (i1, deposits) = unfold(args)
        val i2 = traverseFee(i1)
        val seq = traverseInput(i2)
        Result(seq, deposits)

    case class Args(
        majorVersion: Int,
        // TODO: who is in charge of cutting this list of?
        //  for now let's assume
        deposits: List[DepositUtxo],
        utxosWithdrawn: Map[TransactionInput, TransactionOutput],
        treasuryUtxo: TreasuryUtxo
    )

    case class Result(
        txSeq: SettlementTxSeq,
        deposits: Deposits
    )

    case class Deposits(
        depositsAbsorbed: List[DepositUtxo],
        depositsPostponed: List[DepositUtxo]
    )

    // -------------------------------------------------------------------------
    // 1. unfold
    // -------------------------------------------------------------------------

    def unfold(args: Args): (Intermediate1, Deposits) = {
        // 1. unfold rollout list

        // 2.
        ???
    }

    case class Intermediate1(
        settlementTx: Coin => SettlementTx,
        fallbackTx: TreasuryUtxo => FallbackTx,
        rolloutTxs: List[Coin => (RolloutUtxo => RolloutTx, Coin)]
    )

    // -------------------------------------------------------------------------
    // 2. traverse fee
    // -------------------------------------------------------------------------

    def traverseFee(intermediate1: Intermediate1): Intermediate2 = ???

    case class Intermediate2(
        settlementTx: SettlementTx,
        fallbackTx: TreasuryUtxo => FallbackTx,
        rolloutTxs: List[RolloutUtxo => RolloutTx]
    )

    // -------------------------------------------------------------------------
    // 3. traverse input
    // -------------------------------------------------------------------------

    def traverseInput(intermediate2: Intermediate2): SettlementTxSeq = {
        def completeInput(
            mRolloutUtxo: Option[RolloutUtxo],
            cont: RolloutUtxo => RolloutTx
        ): (Option[RolloutUtxo], RolloutTx) = {
            // TODO: getting from option throws if empty. Need to make `traverseInput` total.
            val tx = cont(mRolloutUtxo.get)
            (tx.rolloutProduced, tx)
        }

        import intermediate2.*

        SettlementTxSeq(
          settlementTx = settlementTx,
          fallbackTx = fallbackTx(settlementTx.treasuryProduced),
          rolloutTxs = rolloutTxs.mapAccumulate(settlementTx.rolloutProduced)(completeInput)._2
        )
    }

    case class SettlementTxSeq(
        settlementTx: SettlementTx,
        fallbackTx: FallbackTx,
        rolloutTxs: List[RolloutTx]
    )

    // -------------------------------------------------------------------------

    object SettlementTx {
        def build(args: Args)(coin: Coin): SettlementTx = {
            ???
        }
    }

    object RolloutTx {
        def build(args: Any)(coin: Coin): (RolloutUtxo => RolloutTx, Coin) = {
            ???
        }
    }

}
