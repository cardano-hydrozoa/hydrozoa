package hydrozoa.multisig.ledger.l1.real

import hydrozoa.multisig.ledger.l1.real.LedgerL1.{TxDeserialized, State}

object LedgerL1 {
    final case class State(
        treasuryUtxo: Unit,
        depositUtxos: List[Unit],
        rolloutUtxos: List[Unit]
        )
    
    final case class TxDeserialized()
    
    def validateTransactionSTS(x: TxDeserialized): Boolean = true
}

final case class LedgerL1(peerPkhs: List[Unit]) {
    val headScript: Unit = ()
    val headAddress: Unit = ()
    val headCurrencySymbol: Unit = ()
    
    sealed trait Transaction {
        val txDeserialized: TxDeserialized = TxDeserialized()
        final val txSerialized: Unit = ()

        def ledgerEndo(s: State): State
    }

    final case class DepositTx() extends Transaction {
        val producedDepositUtxo: Unit = ()

        override def ledgerEndo(s: State): State = s
    }

    final case class FallbackTx() extends Transaction {
        val spentTreasuryUtxo: Unit = ()

        override def ledgerEndo(s: State): State = s
    }

    final case class FinalizationTx() extends Transaction {
        val spentTreasuryUtxo: Unit = ()

        override def ledgerEndo(s: State): State = s
    }

    final case class InitializationTx() extends Transaction {
        val producedTreasuryUtxo: Unit = ()

        override def ledgerEndo(s: State): State = s
    }

    final case class RefundTx() extends Transaction {
        val spentDepositUtxo: Unit = ()

        override def ledgerEndo(s: State): State = s
    }

    final case class RolloutTx() extends Transaction {
        val spentRolloutUtxo: Unit = ()
        val producedRolloutUtxo: Option[Unit] = None

        override def ledgerEndo(s: State): State = s
    }

    final case class SettlementTx() extends Transaction {
        val spentTreasuryUtxo: Unit = ()
        val producedTreasuryUtxo: Unit = ()

        override def ledgerEndo(s: State): State = s
    }
}
