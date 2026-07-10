package hydrozoa.multisig.ledger.l2

import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestId

/** A lightweight record of one applied L2 command, as surfaced by
  * [[EutxoL2LedgerReader.recentTransactions]]. Derived from the L2 ledger's own command log, so
  * every summary corresponds to a command that committed.
  */
final case class L2TxSummary(
    requestId: RequestId,
    blockNumber: BlockNumber,
    kind: L2TxKind
)

/** What an [[L2TxSummary]] records: an applied L2 transaction, or a step in a deposit's lifecycle.
  */
enum L2TxKind:
    case Transaction, DepositRegistered, DepositAbsorbed, DepositRefunded

object L2TxSummary:
    /** Expand one logged real command into its summaries, in the order they appear to a reader. A
      * transaction or a deposit registration is a single summary; a deposit-decisions command is
      * one summary per absorbed deposit followed by one per refunded deposit (so a no-op decisions
      * command with empty lists yields none).
      */
    def fromCommand(command: L2LedgerCommand.Real): Vector[L2TxSummary] = command match
        case c: L2LedgerCommand.ApplyTransaction =>
            Vector(L2TxSummary(c.requestId, c.blockNumber, L2TxKind.Transaction))
        case c: L2LedgerCommand.RegisterDeposit =>
            Vector(L2TxSummary(c.requestId, c.blockNumber, L2TxKind.DepositRegistered))
        case c: L2LedgerCommand.ApplyDepositDecisions =>
            c.absorbedDeposits.toVector
                .map(id => L2TxSummary(id, c.blockNumber, L2TxKind.DepositAbsorbed))
                ++ c.refundedDeposits.toVector
                    .map(id => L2TxSummary(id, c.blockNumber, L2TxKind.DepositRefunded))
