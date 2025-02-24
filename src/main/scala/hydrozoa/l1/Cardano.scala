package hydrozoa.l1

import hydrozoa.{L1Tx, Network, TxId}

trait Cardano {
    def submit(tx: L1Tx): Either[SubmissionError, TxId]
    def network(): Network
}

type SubmissionError = String
