package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.dapp.tx.FallbackTx
import hydrozoa.multisig.ledger.dapp.tx.RolloutTx
import hydrozoa.multisig.ledger.dapp.tx.SettlementTx
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import scalus.cardano.ledger.Coin
import scalus.cardano.ledger.TransactionInput
import scalus.cardano.ledger.TransactionOutput

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

    def unfold(args: Args): (Intermediate1, Deposits) = ???

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

    def traverseInput(intermediate2: Intermediate2): SettlementTxSeq =
        for {
            _ <- ???
        } yield SettlementTxSeq(
          settlementTx = ???,
          fallbackTx = ???,
          rolloutTxs = ???
        )
        
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
