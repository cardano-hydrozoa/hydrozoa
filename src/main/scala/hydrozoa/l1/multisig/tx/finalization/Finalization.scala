package hydrozoa.l1.multisig.tx.finalization

import hydrozoa.l1.multisig.tx.MultisigTxs.FinalizationTx
import hydrozoa.node.server.AwaitingDeposit
import hydrozoa.{AddressBechL1, NativeScript, ParticipantSecretKey, TxIx}

trait FinalizationTxBuilder {
    def mkFinalization(
        recipe: FinalizationRecipe
    ): Either[String, FinalizationTx]
}

case class FinalizationRecipe(
    majorVersion: Int,
    depositsToProtect: Set[AwaitingDeposit]
)
