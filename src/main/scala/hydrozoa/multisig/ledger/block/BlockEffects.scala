package hydrozoa.multisig.ledger.block

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, InitializationTx}
import io.circe.*
import io.circe.generic.semiauto.*

/** Effects for the **initial** block only — the unsigned init + fallback txs the head will sign via
  * the slow consensus's stack-0 hard-ack flow at startup.
  *
  * The Minor / Major / Final variants used to live here; after the fast/slow consensus split those
  * effects (settlement, fallback, rollouts, refunds, finalization) are owned slow-side as
  * `StackEffects.HardConfirmed.Regular` partitions and are no longer block-shaped on the fast
  * cycle. Only the genesis init+fallback pair is retained, paired with `BlockBrief.Initial` via
  * [[hydrozoa.config.head.initialization.InitialBlock]].
  */
sealed trait BlockEffects

object BlockEffects {
    sealed trait Unsigned extends BlockEffects

    object Unsigned {
        final case class Initial(
            initializationTx: InitializationTx,
            fallbackTx: FallbackTx,
        ) extends BlockEffects.Unsigned

        // Encoder only — InitializationTx / FallbackTx have CBOR-hex encoders but no decoders
        // (they require semantic reconstruction from a raw `Transaction` plus bootstrap context,
        // done in `HeadConfig.headConfigDecoder`).
        given (using CardanoNetwork.Section): Encoder[Initial] = deriveEncoder[Initial]
    }
}
