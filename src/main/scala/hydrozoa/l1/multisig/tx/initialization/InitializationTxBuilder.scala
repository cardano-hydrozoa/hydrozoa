package hydrozoa.l1.multisig.tx.initialization

import hydrozoa.*
import hydrozoa.l1.multisig.tx.InitTx
import scalus.cardano.address.Address
import scalus.cardano.ledger.Transaction

// TODO: Make the Address fit better into the hydrazoa type heirarchy 
// (i.e., this should read InitTx instead of Transaction
trait InitTxBuilder {
    def mkInitializationTxDraft(
        recipe: InitTxRecipe
    ): Either[String, (InitTx, AddressBechL1)]
}

case class InitTxRecipe(
    network: Network,
    seedUtxo: UtxoIdL1,
    coins: BigInt,
    peers: Set[VerificationKeyBytes],
)
