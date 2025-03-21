package hydrozoa.l1.multisig.tx.initialization

import hydrozoa.*
import hydrozoa.l1.multisig.tx.InitializationTx

trait InitTxBuilder {
    def mkInitializationTxDraft(
        recipe: InitTxRecipe
    ): Either[String, (InitializationTx, AddressBechL1)]
}

case class InitTxRecipe(
    headAddressBech32: String,
    seedOutput: OutputRefL1,
    coins: Long,
    headNativeScript: NativeScript,
    beaconTokenName: String
)
