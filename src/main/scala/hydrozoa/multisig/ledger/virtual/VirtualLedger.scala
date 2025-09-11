package hydrozoa.multisig.ledger.virtual

import VirtualLedger.{Config, ErrorApplyInternalTx, ErrorApplyWithdrawalTx, State, Tx}
import cats.effect.{IO, Ref}
import hydrozoa.multisig.ledger.virtual.tx.GenesisTx
import scalus.cardano.ledger.{Transaction, TransactionOutput}

type KzgCommitment = String

final case class VirtualLedger(config: Config)(private val state: Ref[IO, State]) {
    
    def applyInternalTx(txSerialized: Tx.Serialized): IO[Either[ErrorApplyInternalTx, Unit]] =
        ???
        
    def applyWithdrawalTx(txSerialized: Tx.Serialized): IO[Either[ErrorApplyWithdrawalTx, List[TransactionOutput]]] =
        ???
        
    def applyGenesisTx(tx: GenesisTx): IO[Unit] =
        ???
        
    def getKzgCommitment: IO[KzgCommitment] =
        ???
}

/** ==Hydrozoa's open virtual ledger in the multisig regime==
  *
  * '''Virtual ledger''' means that while its exogenously initialized state may correspond to some
  * underlying state in another ledger, the virtual ledger's state can transition without
  * necessarily corresponding to any transitions in the underlying ledger. For example, Hydrozoa's
  * internal transactions spend and produce utxos in the virtual ledger, but these changes need not
  * be reconciled with any other ledgers.
  *
  * '''Open ledger''' means that the total assets in the ledger's state can fluctuate in other ways
  * besides the initial/final ledger transitions and endogenous asset minting/burning ledger rules.
  */
object VirtualLedger {
    final case class Config(
        protocolParams: Unit
    )

    final case class State(
        activeUtxos: List[Unit]
    )

    sealed trait Tx {
        val tx: Transaction
    }
    
    object Tx {
        type Serialized = Array[Byte]
    }
    
    sealed trait ErrorApplyInternalTx
    
    sealed trait ErrorApplyWithdrawalTx
}
