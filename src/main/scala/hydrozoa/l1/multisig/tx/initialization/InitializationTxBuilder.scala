package hydrozoa.l1.multisig.tx.initialization

import hydrozoa.*
import hydrozoa.l1.multisig.tx.MultisigTxs.InitializationTx

trait InitTxBuilder {
    def mkInitializationTxDraft(
        recipe: InitTxRecipe
    ): Either[String, (InitializationTx, AddressBechL1)]
}

case class InitTxRecipe(
    headAddressBech32: String,
    txId: TxId,
    txIx: TxIx,
    amount: Long,
    headNativeScript: NativeScript,
    beaconTokenName: String
)
