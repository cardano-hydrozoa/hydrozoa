package hydrozoa.l1.multisig.tx.initialization

import hydrozoa.*
import hydrozoa.l1.multisig.tx.InitTx
import scalus.cardano.address.Network
import scalus.cardano.ledger.{Coin, Transaction}

// TODO: Make the Address fit better into the hydrazoa type heirarchy
// (i.e., this should read InitTx instead of Transaction
trait InitTxBuilder {
    def mkInitializationTxDraft(
        recipe: InitTxRecipe
    ): Either[String, (InitTx, AddressL1)]
}

case class InitTxRecipe(
    network: Network,
    seedUtxo: UtxoIdL1,
    coins: Coin,
    peers: Set[VerificationKeyBytes]
)
