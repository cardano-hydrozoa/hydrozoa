package hydrozoa.multisig.ledger.eutxol2

import scalus.cardano.ledger.{MultiAsset, TransactionInput, Utxos}

/** The transient-token compartment: utxo id → the transient token bundle overlaid on that
  * main-compartment utxo. Bundles are [[MultiAsset]], so ADA can never appear here by construction.
  * Invariant: every key also exists in the main compartment — an overlay entry never outlives its
  * utxo.
  */
type TransientTokens = Map[TransactionInput, MultiAsset]

object TransientTokens {
    val empty: TransientTokens = Map.empty

    /** Merge the overlay onto the main compartment: each overlaid utxo's value gains its transient
      * bundle. This is the view the ledger rules and scripts run against. Overlay keys absent from
      * `main` are ignored (the compartment invariant rules them out).
      */
    def mkCombinedUtxos(main: Utxos, transientTokens: TransientTokens): Utxos =
        transientTokens.foldLeft(main) { case (acc, (input, bundle)) =>
            acc.updatedWith(input)(
              _.map(output =>
                  output.withValue(output.value.copy(assets = output.value.assets + bundle))
              )
            )
        }

    /** Inverse of [[mkCombinedUtxos]] over the overlay's keys: subtract each bundle from the
      * combined utxo's value, recovering the main compartment.
      */
    def projectMainUtxos(combined: Utxos, transientTokens: TransientTokens): Utxos =
        transientTokens.foldLeft(combined) { case (acc, (input, bundle)) =>
            acc.updatedWith(input)(
              _.map(output =>
                  output.withValue(output.value.copy(assets = output.value.assets - bundle))
              )
            )
        }
}

/** The two value compartments of the L2 ledger: the main compartment holds L1-valid utxos (what the
  * evacuation map reflects); the transient-token compartment overlays it.
  */
final case class Compartments(main: Utxos, transientTokens: TransientTokens)
