package hydrozoa.multisig.ledger.l1.real.tx

import hydrozoa.multisig.ledger.l1.real.LedgerL1.Tx
import hydrozoa.multisig.ledger.l1.real.utxo.TreasuryUtxo
import scalus.cardano.ledger.{Coin, Transaction}
import hydrozoa.{Address, L1, Utxo}

import hydrozoa.multisig.ledger.l1.real.token.Token.mkHeadTokenName

final case class InitializationTx(
    treasuryProduced: TreasuryUtxo,
    override val tx: Transaction
) extends Tx

object InitializationTx {
    final case class Recipe(
        headAddress: Address[L1],
        fundingUtxos: List[Utxo[L1]],
        fundingToTreasury: Coin
    )

    sealed trait BuildError extends Throwable

    def build(recipe: Recipe): Either[BuildError, InitializationTx] = ???
}
