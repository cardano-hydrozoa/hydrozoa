package hydrozoa.multisig.ledger.l1.tx

import monocle.{Focus, Lens}
import scalus.cardano.ledger.Transaction
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

/** [[EnrichedTx]] escape hatch for submissions that don't belong to any hydrozoa protocol family —
  * user-facing wallet moves, bootstrap-time admin txs, and test probes that submit raw bytes.
  */
final case class RawTx(
    override val tx: Transaction,
    override val txLens: Lens[RawTx, Transaction] = Focus[RawTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty,
) extends EnrichedTx[RawTx]

object RawTx {
    given TxFamily[RawTx] = TxFamily.of("RawTx")
}
