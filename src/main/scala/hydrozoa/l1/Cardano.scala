package hydrozoa.l1

import hydrozoa.{L1Tx, Network, TxId}
import scalus.ledger.api.v1.PosixTime

trait Cardano {
    def submit(tx: L1Tx): Either[SubmissionError, TxId]
    def network(): Network
    def lastBlockTime: PosixTime
}

type SubmissionError = String
