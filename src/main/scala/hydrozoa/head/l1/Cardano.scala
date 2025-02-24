package hydrozoa.head.l1

import hydrozoa.head.{Network, L1Tx, TxId}

trait Cardano {
    def submit(tx: L1Tx): Either[SubmissionError, TxId]
    def network(): Network
}

type SubmissionError = String
