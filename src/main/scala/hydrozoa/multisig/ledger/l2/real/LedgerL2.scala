package hydrozoa.multisig.ledger.l2.real

import hydrozoa.multisig.ledger.l2.real.LedgerL2.State

object LedgerL2 {
    final case class Config(
        protocolParams: Unit
    )

    final case class State(
        activeUtxos: List[Unit]
    )

    sealed trait Transaction

    final case class InnerTx() extends Transaction
    final case class WithdrawalTx() extends Transaction
    final case class GenesisTx() extends Transaction
    final case class ApocalypseTx() extends Transaction
}

final case class LedgerL2(s: State) {

}
