package hydrozoa.l1

import hydrozoa.{Network, TxId, TxL1}
import scalus.ledger.api.v1.PosixTime

trait CardanoL1 {
    def submit(tx: TxL1): Either[SubmissionError, TxId]
    def awaitTx(txId: TxId): Unit
    def network: Network
    def lastBlockTime: PosixTime
}

type SubmissionError = String
