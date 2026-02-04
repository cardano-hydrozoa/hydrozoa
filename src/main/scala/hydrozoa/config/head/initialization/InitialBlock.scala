package hydrozoa.config.head.initialization

import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.block.Block
import hydrozoa.multisig.ledger.dapp.tx.{FallbackTx, InitializationTx}
import hydrozoa.multisig.ledger.dapp.utxo.MultisigRegimeUtxo
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment

final case class InitialBlock(
    override val initialBlock: Block.MultiSigned.Initial
) extends InitialBlock.Section {}

object InitialBlock {
    trait Section {
        def initialBlock: Block.MultiSigned.Initial

        transparent inline def headStartTime: QuantizedInstant = initialBlock.startTime

        transparent inline def initialKzgCommitment: KzgCommitment = initialBlock.kzgCommitment

        transparent inline def initializationTx: InitializationTx = initialBlock.initializationTx
        transparent inline def initialFallbackTx: FallbackTx = initialBlock.fallbackTx

        transparent inline def multisigRegimeUtxo: MultisigRegimeUtxo =
            initializationTx.multisigRegimeUtxo
    }
}
