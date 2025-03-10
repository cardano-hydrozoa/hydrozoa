package hydrozoa.l1.multisig.tx.initialization

import hydrozoa.{L1Tx, NativeScript, TxId, TxIx}

trait InitTxBuilder {
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
