package hydrozoa.l1.multisig.tx.finalization

import hydrozoa.l1.multisig.tx.MultisigTxs.FinalizationTx
import hydrozoa.{AddressBechL1, NativeScript, ParticipantSecretKey, TxIx}

trait FinalizationTxBuilder {
    def buildFinalizationTxDraft(
        recipe: FinalizationRecipe
    ): Either[String, FinalizationTx]
}

case class FinalizationRecipe(
    majorVersion: Int
)
