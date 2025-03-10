package hydrozoa.l1.multisig.tx.initialization

import hydrozoa.*

trait InitTxBuilder {
    def mkInitDraft(recipe: InitTxRecipe): Either[String, (L1Tx, AddressBechL1)]
}

case class InitTxRecipe(
    headAddressBech32: String,
    txId: TxId,
    txIx: TxIx,
    amount: Long,
    headNativeScript: NativeScript,
    beaconTokenName: String
)
