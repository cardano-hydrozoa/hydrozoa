package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.State
import cats.implicits.*
import hydrozoa.multisig.ledger.dapp.tx.{FallbackTx, RolloutTx, SettlementTx}
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, RolloutUtxo, TreasuryUtxo}
import scalus.cardano.ledger.{Coin, TransactionInput, TransactionOutput}
import SettlementTx.Recipe

object SettlementTxSeq {

    def build(args: Recipe): Result =
        val (i1, deposits) = unfold(args)
        val i2 = traverseFee(i1)
        val seq = traverseInput(i2)
        Result(seq, deposits)

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

    def unfold(args: Recipe): (Intermediate1, Deposits) = {
        // TODO: some args for the fallback tx builder
        val fallback: NewBuild.FallbackTx.PartialResult = NewBuild.FallbackTx.build()

        val settlement: NewBuild.SettlementTx.PartialResult = NewBuild.SettlementTx.build(args)

        val rollouts = List.unfold(settlement.remainingPayouts)(NewBuild.RolloutTx.unfoldNext)

        (
          Intermediate1(settlement.cont, fallback._2, rollouts),
          settlement.deposits
        )
    }

    case class Intermediate1(
        settlementTx: NewBuild.SettlementTx.Cont,
        fallbackTx: NewBuild.FallbackTx.Cont,
        rolloutTxs: List[NewBuild.RolloutTx.Cont1]
    )

    // -------------------------------------------------------------------------
    // 2. traverse fee
    // -------------------------------------------------------------------------

    def traverseFee(intermediate1: Intermediate1): Intermediate2 = {
        import intermediate1.*

        val (totalCoin, newRolloutTxs) =
            rolloutTxs.reverse.mapAccumulate(Coin(0))((x, cont) => cont(x))

        Intermediate2(
          settlementTx = settlementTx(totalCoin),
          fallbackTx = fallbackTx,
          rolloutTxs = newRolloutTxs.reverse
        )
    }

    case class Intermediate2(
        settlementTx: SettlementTx,
        fallbackTx: TreasuryUtxo => FallbackTx,
        rolloutTxs: List[NewBuild.RolloutTx.Cont2]
    )

    // -------------------------------------------------------------------------
    // 3. traverse input
    // -------------------------------------------------------------------------

    def traverseInput(intermediate2: Intermediate2): SettlementTxSeq = {
        import intermediate2.*

        // TODO: getting from option throws if empty. Need to make `traverseInput` total.
        val (_, newRolloutTxs) =
            rolloutTxs.mapAccumulate(settlementTx.rolloutProduced)((x, cont) => cont(x.get))

        SettlementTxSeq(
          settlementTx = settlementTx,
          fallbackTx = fallbackTx(settlementTx.treasuryProduced),
          rolloutTxs = newRolloutTxs
        )
    }

    case class SettlementTxSeq(
        settlementTx: SettlementTx,
        fallbackTx: FallbackTx,
        rolloutTxs: List[RolloutTx]
    )

    // -------------------------------------------------------------------------

    object NewBuild {
        object SettlementTx {
            type Cont = Coin => SettlementTx

            case class PartialResult(
                remainingPayouts: Map[TransactionInput, TransactionOutput],
                deposits: Deposits,
                cont: Cont
            )

            def build(args: Recipe): PartialResult = {
                ???
            }
        }

        object FallbackTx {
            type Cont = TreasuryUtxo => FallbackTx

            type PartialResult = (
                Coin,
                Cont
            )

            def build(): PartialResult = ???
        }

        object RolloutTx {
            type Cont1 = Coin => PartialResult2

            type PartialResult1 = (
                Cont1,
                Map[TransactionInput, TransactionOutput]
            )

            type Cont2 = RolloutUtxo => (Option[RolloutUtxo], RolloutTx)

            type PartialResult2 = (
                Coin,
                Cont2,
            )

            def build(args: Any): PartialResult1 = {
                ???
            }

            def unfoldNext(
                remainingPayouts: Map[TransactionInput, TransactionOutput]
            ): Option[PartialResult1] =
                ???
        }
    }
}
