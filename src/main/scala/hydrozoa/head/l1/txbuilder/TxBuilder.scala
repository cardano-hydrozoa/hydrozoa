package hydrozoa.head.l1.txbuilder

import hydrozoa.head.*

trait TxBuilder {
    def mkInitDraft(recipe: InitTxRecipe): Either[String, L1Tx]
}

case class InitTxRecipe(
    headAddressBech32: String,
    txId: TxId,
    txIx: TxIx,
    amount: Long,
    headNativeScript: NativeScript,
    beaconTokenName: String
)
