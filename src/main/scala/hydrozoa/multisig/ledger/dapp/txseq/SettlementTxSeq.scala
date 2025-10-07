package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.dapp.tx.FallbackTx
import hydrozoa.multisig.ledger.dapp.tx.RolloutTx
import hydrozoa.multisig.ledger.dapp.tx.SettlementTx
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, RolloutUtxo, TreasuryUtxo}
import scalus.cardano.ledger.{Coin, TransactionInput, TransactionOutput}

object SettlementTxSeq {

    case class Args(
        majorVersion: Int,
        // TODO: who is in charge of cutting this list of?
        //  for now let's assume
        deposits: List[DepositUtxo],
        utxosWithdrawn: Map[TransactionInput, TransactionOutput],
        treasuryUtxo: TreasuryUtxo
    )
    
    case class Intermediate1(
        settlementTx: Coin => SettlementTx,
        fallbackTx: TreasuryUtxo => FallbackTx,
        rolloutTxs: List[Coin => (RolloutUtxo => RolloutTx, Coin)]
    )
    
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

    case class Result(
        settlementTx: SettlementTx,
        fallbackTx: FallbackTx,
        rolloutTxs: List[RolloutTx],
        depositsAbsorbed: NonEmptyList[DepositUtxo],
        depositsPostponed: List[DepositUtxo]
    )

    def run(recipe: SettlementTx.Recipe) = {

        // Unfinished settlement tx with
        //  - treasury input/output
        //  - native script (implicitly)
        //  - dummy signatures (implicitly)
        //  - limited number of deposits as tx inputs
        //  - dummy maximum rollout output
        //  - as many withdrawals

        /*

     (AdaRequired -> SettlementT,
      [AdaRequired -> RolloutTx],
      ProducedTreasury -> FallbackTx
     )

         */

    }
}
