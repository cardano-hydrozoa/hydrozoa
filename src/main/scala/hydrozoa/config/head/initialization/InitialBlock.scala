package hydrozoa.config.head.initialization

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockEffects}
import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, InitializationTx}
import hydrozoa.multisig.ledger.l1.utxo.MultisigRegimeUtxo
import io.circe.*
import io.circe.generic.semiauto.*

/** Genesis "block 0" payload carried by [[hydrozoa.config.head.HeadConfig]]: the initial brief
  * (timings + header) plus the **unsigned** initialization + fallback transactions.
  *
  * At startup, slow consensus stack-0 takes the unsigned txs (via `StackEffects.Unsigned.Initial`)
  * and runs its hard-ack flow to multisign them. The config no longer pre-signs anything — all
  * peers receive the same unsigned shape.
  */
final case class InitialBlock(
    blockBrief: BlockBrief.Initial,
    effects: BlockEffects.Unsigned.Initial
) extends InitialBlock.Section {
    override transparent inline def initialBlockSection: InitialBlock = this
}

object InitialBlock {
    // Encoder only — see `BlockEffects.Unsigned.Initial` codec note.
    given (using CardanoNetwork.Section): Encoder[InitialBlock] = deriveEncoder[InitialBlock]

    trait Section {
        def initialBlockSection: InitialBlock
        def initialBlock: InitialBlock = initialBlockSection

        def initializationTx: InitializationTx = initialBlock.effects.initializationTx
        def initialFallbackTx: FallbackTx = initialBlock.effects.fallbackTx

        def multisigRegimeUtxo: MultisigRegimeUtxo =
            initializationTx.multisigRegimeProduced
    }
}
