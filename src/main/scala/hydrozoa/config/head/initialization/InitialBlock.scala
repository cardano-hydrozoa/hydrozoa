package hydrozoa.config.head.initialization

import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.block.Block
import hydrozoa.multisig.ledger.dapp.tx.{FallbackTx, InitializationTx}
import hydrozoa.multisig.ledger.dapp.utxo.MultisigRegimeUtxo
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment

final case class InitialBlock(
    override val initialBlock: Block.MultiSigned.Initial
) extends InitialBlock.Section {
    override transparent inline def initialBlockSection: InitialBlock = this
}

object InitialBlock {
    trait Section {
        def initialBlockSection: InitialBlock
        def initialBlock: Block.MultiSigned.Initial

        def headStartTime: QuantizedInstant = initialBlock.startTime

        def initialKzgCommitment: KzgCommitment = initialBlock.kzgCommitment

        def initializationTx: InitializationTx = initialBlock.initializationTx
        def initialFallbackTx: FallbackTx = initialBlock.fallbackTx

        def multisigRegimeUtxo: MultisigRegimeUtxo =
            initializationTx.multisigRegimeProduced
    }
}
