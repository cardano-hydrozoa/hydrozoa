package hydrozoa.l1.multisig.tx.initialization

import hydrozoa.*
import hydrozoa.l1.multisig.tx.InitTx

trait InitTxBuilder {
    def mkInitializationTxDraft(
        recipe: InitTxRecipe
    ): Either[String, (InitTx, AddressBechL1)]
}

case class InitTxRecipe(
    headAddressBech32: AddressBechL1,
    seedOutput: UtxoIdL1,
    coins: Long,
    headNativeScript: NativeScript,
    beaconTokenName: TokenName
)
